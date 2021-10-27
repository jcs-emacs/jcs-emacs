;;; jcs-hook.el --- All the hook event do here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Focus In/Out" )
;;

(defvar jcs-foucs-after-first-p nil
  "Flag to see if after first focus.")

(defun jcs-hook--focus-in ()
  "When window is focus."
  (if (not jcs-foucs-after-first-p)
      (setq jcs-foucs-after-first-p t)
    (jcs-safe-revert-all-buffers)
    (jcs-funcall-fboundp #'jcs--feebleline--reset)))

(defun jcs-hook--focus-out ()
  "When window is not focus."
  )

(defun jcs-after-focus-change-function ()
  "Focus in/out function."
  (if (frame-focus-state) (jcs-hook--focus-in) (jcs-hook--focus-out)))
(add-function :after after-focus-change-function #'jcs-after-focus-change-function)

(defun jcs-window-size-change-functions (&rest _)
  "When window changed size."
  (jcs-dashboard--window-size-change)
  (when (featurep 'jcs-ivy) (jcs-ivy--window-size-change))
  (when (featurep 'treemacs) (jcs-treemacs--window-size-change)))
(add-hook 'window-size-change-functions 'jcs-window-size-change-functions)

;;
;; (@* "Find Files" )
;;

(defun jcs-hook--find-file ()
  "Find file hook."
  (jcs-update-buffer-save-string)
  (jcs-active-line-numbers-by-mode)
  (jcs-project-remember)
  (jcs-project--track-open-projects))
(add-hook 'find-file-hook 'jcs-hook--find-file)

(defun jcs--find-file--advice-after (&rest _)
  "Advice execute after command `find-file'."
  (when jcs-current-created-parent-dir-path
    (setq jcs-created-parent-dir-path jcs-current-created-parent-dir-path
          jcs-current-created-parent-dir-path nil))
  (jcs-buffer-menu-safe-refresh)
  (jcs-dashboard-safe-refresh-buffer))
(advice-add 'find-file :after #'jcs--find-file--advice-after)

(defun jcs--switch-to-buffer--advice-after (&rest _)
  "Advice execute after command `switch-to-buffer'."
  (jcs-dashboard-safe-refresh-buffer)
  (jcs-buffer-menu-safe-refresh))
(advice-add 'switch-to-buffer :after #'jcs--switch-to-buffer--advice-after)

(defun jcs-hook--other-window-interactively-p ()
  "Return non-nil, if executing `other-window'."
  (memq this-command '(other-window jcs-other-window-prev jcs-other-window-next)))

(defun jcs--other-window--advice-before (&rest _)
  "Advice execute before `other-window' command."
  (when (jcs-hook--other-window-interactively-p)
    (jcs-funcall-fboundp 'company-abort)))
(advice-add 'other-window :before #'jcs--other-window--advice-before)

(defun jcs--other-window--advice-after (count &rest _)
  "Advice execute after command `other-window'."
  ;; NOTE: If it's a utility frame; then we skip it immediately.
  (cond ((jcs-frame-util-p)
         (other-window (if (jcs-is-positive count) 1 -1) t))
        ((jcs-hook--other-window-interactively-p)
         (select-frame-set-input-focus (selected-frame))
         (jcs-buffer-menu-safe-refresh)
         (jcs-dashboard-safe-refresh-buffer))))
(advice-add 'other-window :after #'jcs--other-window--advice-after)

;;
;; (@* "First load" )
;;

(defun jcs--fl--find-file--advice-before (&rest _)
  "Advice execute before command `find-file'."
  ;; Fixed `css-mode' opening virtual buffer with directory error. You just
  ;; need to preload this before actually create the virtual buffer.
  (require 'eww nil t)
  (advice-remove 'find-file #'jcs--fl--find-file--advice-before))

(advice-add 'find-file :before #'jcs--fl--find-file--advice-before)

;;
;; (@* "Initialization" )
;;

(defun jcs-hook--after-init ()
  "Hook run after initialize."
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;; NOTE: Load required packages here.
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (progn
    (require 'company)
    (require 'dashboard)
    (require 'diminish)
    (require 'highlight-indent-guides)
    (require 'ivy)
    (require 'yascroll))

  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;; NOTE: Enable util modes here.
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (progn
    ;;----------------------------------- `auto-highlight-symbol'
    (global-auto-highlight-symbol-mode t)
    ;;----------------------------------- `hl-line'
    (global-hl-line-mode 1)
    ;;----------------------------------- `indent-control'
    (indent-control-mode 1)
    ;;----------------------------------- `ivy'
    (ivy-mode 1)
    ;;----------------------------------- `powerline'
    (powerline-default-theme)
    ;;----------------------------------- `tree-sitter'
    (global-tree-sitter-mode 1)
    ;;----------------------------------- `use-ttf'
    (use-ttf-set-default-font)
    ;;----------------------------------- `yascroll'
    (global-yascroll-bar-mode 1))

  (jcs-setup-default-theme)
  (jcs-command-mode) (jcs-depend-mode)

  ;; Font Size
  (jcs-set-font-size jcs-default-font-size)

  ;; Frame Title
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

  ;; NOTE: Lower the `GC' back to normal threshold.
  (jcs-gc-cons-threshold-speed-up nil)
  (setq file-name-handler-alist jcs-file-name-handler-alist)

  ;; IMPORTANT: This should always be the last thing.
  (jcs-dashboard-init-info))
(add-hook 'after-init-hook 'jcs-hook--after-init)

;;
;; (@* "Pre/Post Command" )
;;

(defun jcs-hook--pre-command ()
  "Hook run before every command."
  (jcs--er/record-history))
(add-hook 'pre-command-hook 'jcs-hook--pre-command)

(defun jcs-hook--post-command ()
  "Hook run after every command."
  (jcs--er/resolve-region)
  (jcs--mark-whole-buffer-resolve)
  (jcs-reload-active-mode-with-error-handle)
  (unless (display-graphic-p) (jcs-feebleline-display-mode-line-graphic)))
(add-hook 'post-command-hook 'jcs-hook--post-command)

(defun jcs-hook--first-pre-command ()
  "Pre command that only run once."
  (global-alt-codes-mode 1)
  (auto-read-only-mode 1)
  (delete-selection-mode 1)
  (global-docstr-mode 1)
  (global-hl-todo-mode 1)
  (global-page-break-lines-mode 1)
  (global-region-occurrences-highlighter-mode 1)
  (right-click-context-mode 1)
  (show-paren-mode t)
  (global-so-long-mode 1)
  (which-key-mode)
  (remove-hook 'pre-command-hook 'jcs-hook--first-pre-command))
(add-hook 'pre-command-hook 'jcs-hook--first-pre-command)

;;
;; (@* "Major Mode" )
;;

(defun jcs-hook--after-change-major-mode ()
  "Hook run after major mode changes."
  (jcs-active-line-numbers-by-mode))
(add-hook 'after-change-major-mode-hook 'jcs-hook--after-change-major-mode)

;;
;; (@* "Quitting" )
;;

(defun jcs-hook--kill-emacs ()
  "Hook run before Emacs is killed."
  (when (featurep 'ffmpeg-player) (ignore-errors (ffmpeg-player-clean))))
(add-hook 'kill-emacs-hook 'jcs-hook--kill-emacs)

(defun jcs--quit-command (&rest _)
  "Advice for quit command."
  (deactivate-mark)
  (jcs-process-reporter-done))

(advice-add 'keyboard-quit :before #'jcs--quit-command)
(advice-add 'top-level :before #'jcs--quit-command)

;;
;; (@* "Startup" )
;;

(defvar jcs-emacs-ready-p nil
  "Flag to check if Emacs is ready.")

(defvar jcs-emacs-startup-directory nil
  "Record the startup directory.")

(defun jcs-hook--emacs-startup ()
  "Hook run after Emacs is startup."
  (with-current-buffer jcs-scratch-buffer-name
    (setq jcs-scratch--content (buffer-string)))
  (setq jcs-emacs-ready-p t
        jcs-emacs-startup-directory default-directory))
(add-hook 'emacs-startup-hook 'jcs-hook--emacs-startup)

(provide 'jcs-hook)
;;; jcs-hook.el ends here
