;;; jcs-hook.el --- All the hook event do here.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Focus In/Out" )
;;

(defun jcs-focus-in-hook ()
  "When window is focus."
  (jcs-revert-all-file-buffers)
  (jcs-funcall-fboundp #'jcs--feebleline--reset))
(add-hook 'focus-in-hook 'jcs-focus-in-hook)

(defun jcs-focus-out-hook ()
  "When window is not focus."
  )
(add-hook 'focus-out-hook 'jcs-focus-out-hook)

(defun jcs-window-size-change-functions (&rest _)
  "When window changed size."
  (setq ivy-height (round (* (frame-height) jcs-ivy-height-ratio)))
  (when (and (window-minibuffer-p) jcs-ivy-enabled-p)
    (jcs-mute-apply
      (ivy--resize-minibuffer-to-fit)
      (ivy-shrink-after-dispatching)
      (ivy--exhibit))))
(add-hook 'window-size-change-functions 'jcs-window-size-change-functions)

;;
;; (@* "Find Files" )
;;

(defun jcs-find-file-hook ()
  "Find file hook."
  (unless (jcs-reload-emacs-reloading-p) (jcs-active-line-numbers-by-mode)))
(add-hook 'find-file-hook 'jcs-find-file-hook)

(defun jcs--find-file--advice-before (&rest _)
  "Advice execute before `find-file' command."
  ;; Fixed `css-mode' opening virtual buffer with directory error. You just
  ;; need to preload this before actually create the virtual buffer.
  (ignore-errors (require 'eww)))
(advice-add 'find-file :before #'jcs--find-file--advice-before)

(defun jcs--find-file--advice-after (&rest _)
  "Advice execute after `find-file' command."
  (when jcs-current-created-parent-dir-path
    (setq jcs-created-parent-dir-path jcs-current-created-parent-dir-path)
    (setq jcs-current-created-parent-dir-path nil))
  (jcs--neotree-start-refresh)
  (jcs-buffer-menu-safe-refresh)
  (jcs-dashboard-safe-refresh-buffer))
(advice-add 'find-file :after #'jcs--find-file--advice-after)

(defun jcs--switch-to-buffer--advice-around (fnc &rest args)
  "Advice execute around `switch-to-buffer' function."
  (apply fnc args)
  (unless (jcs-buffer-shown-p dashboard-buffer-name)
    (jcs-dashboard-safe-refresh-buffer)))
(advice-add 'switch-to-buffer :around #'jcs--switch-to-buffer--advice-around)

(defun jcs--switch-to-buffer--advice-after (&rest _)
  "Advice execute after `switch-to-buffer' command."
  (jcs--neotree-start-refresh)
  (jcs-buffer-menu-safe-refresh))
(advice-add 'switch-to-buffer :after #'jcs--switch-to-buffer--advice-after)

(defun jcs--other-window--advice-before (&rest _)
  "Advice execute before `other-window' command."
  (unless jcs--no-advice-other-window
    (when (fboundp 'company-abort) (company-abort))))
(advice-add 'other-window :before #'jcs--other-window--advice-before)

(defun jcs--other-window--advice-after (count &rest _)
  "Advice execute after `other-window' command."
  ;; NOTE: If it's a utility frame; then we skip it immediately.
  (when (jcs-frame-util-p)
    (other-window (if (jcs-is-positive count) 1 -1) t))
  (unless jcs--no-advice-other-window
    (select-frame-set-input-focus (selected-frame))
    (jcs--neotree-start-refresh)
    (when (and (boundp 'neo-buffer-name)
               (not (string= neo-buffer-name (buffer-name (current-buffer)))))
      (setq jcs--neotree--last-window (selected-window)))
    (jcs-buffer-menu-safe-refresh)
    (jcs-dashboard-safe-refresh-buffer)))
(advice-add 'other-window :after #'jcs--other-window--advice-after)

;;
;; (@* "Initialization" )
;;

(defun jcs-after-init-hook ()
  "Hook run after initialize."
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;; NOTE: Load required packages here.
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (progn
    (require 'alt-codes)
    (require 'auto-highlight-symbol)
    (require 'auto-read-only)
    (require 'company)
    (require 'dashboard)
    (require 'diminish)
    (require 'exec-path-from-shell)
    (require 'highlight-indent-guides)
    (require 'hl-line)
    (require 'hl-todo)
    (require 'ivy)
    (require 'page-break-lines)
    (require 'powerline)
    (require 'preproc-font-lock)
    (require 'region-occurrences-highlighter)
    (require 'right-click-context)
    (require 'use-ttf)
    (require 'which-key)
    (require 'yascroll))

  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;; NOTE: Enable util modes here.
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (progn
    ;;----------------------------------- `alt-codes'
    (global-alt-codes-mode 1)
    ;;----------------------------------- `auto-highlight-symbol'
    (global-auto-highlight-symbol-mode t)
    ;;----------------------------------- `auto-read-only'
    (auto-read-only-mode 1)
    ;;----------------------------------- `delete-selection'
    (delete-selection-mode 1)
    ;;----------------------------------- `goto-address'
    (goto-address-mode t)
    ;;----------------------------------- `hl-line'
    (global-hl-line-mode 1)
    ;;----------------------------------- `hl-todo'
    (global-hl-todo-mode 1)
    ;;----------------------------------- `ivy'
    (ivy-mode 1)
    ;;----------------------------------- `page-break-lines'
    (global-page-break-lines-mode 1)
    ;;----------------------------------- `powerline'
    (powerline-default-theme)
    ;;----------------------------------- `preproc-font-lock'
    (preproc-font-lock-global-mode t)
    (preproc-font-lock-mode t)
    ;;----------------------------------- `projectile'
    (projectile-mode t)
    ;;----------------------------------- `region-occurrences-highlighter'
    (global-region-occurrences-highlighter-mode 1)
    ;;----------------------------------- `right-click-context'
    (right-click-context-mode 1)
    ;;----------------------------------- `show-paren'
    ;; NOTE: turn on highlight matching brackets when cursor is on one
    (show-paren-mode t)
    ;;----------------------------------- `use-ttf'
    (use-ttf-set-default-font)
    ;;----------------------------------- `which-key'
    (which-key-mode)
    ;;----------------------------------- `yascroll'
    (global-yascroll-bar-mode 1))

  (jcs-setup-default-theme)
  (jcs-command-mode)
  (jcs-depend-mode)

  (jcs-reload-file-info)
  (jcs-reload-docstring-info)

  (menu-bar-mode -1)
  (when (display-graphic-p) (scroll-bar-mode -1))
  (tool-bar-mode -1)

  ;; Language Environment
  (set-language-environment jcs-language-environment)

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
  (setq dashboard-init-info
        (format "[ %s ] [ Total took %0.1f seconds ]"
                dashboard-init-info
                (string-to-number (emacs-init-time)))))
(add-hook 'after-init-hook 'jcs-after-init-hook)

;;
;; (@* "Pre/Post Command" )
;;

(defun jcs-pre-command-hook ()
  "Hook run before every command."
  )
(add-hook 'pre-command-hook 'jcs-pre-command-hook)

(defun jcs-post-command-hook ()
  "Hook run after every command."
  (jcs--mark-whole-buffer-resolve)
  (jcs-reload-active-mode-with-error-handle)
  (unless (display-graphic-p) (jcs-feebleline-display-mode-line-graphic)))
(add-hook 'post-command-hook 'jcs-post-command-hook)

;;
;; (@* "Major Mode" )
;;

(defun jcs-after-change-major-mode-hook ()
  "Hook run after major mode changes."
  (unless (jcs-reload-emacs-reloading-p) (jcs-active-line-numbers-by-mode)))
(add-hook 'after-change-major-mode-hook 'jcs-after-change-major-mode-hook)

;;
;; (@* "Quitting" )
;;

(defun jcs--kill-emacs-hook ()
  "Hook run before Emacs is killed."
  (when (fboundp 'ffmpeg-player-clean) (ffmpeg-player-clean)))
(add-hook 'kill-emacs-hook 'jcs--kill-emacs-hook)

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

(defun jcs--emacs-startup-hook ()
  "Hook run after Emacs is startup."
  (with-current-buffer "*scratch*" (setq jcs-scratch--content (buffer-string)))
  (setq jcs-emacs-ready-p t))
(add-hook 'emacs-startup-hook 'jcs--emacs-startup-hook)

(provide 'jcs-hook)
;;; jcs-hook.el ends here
