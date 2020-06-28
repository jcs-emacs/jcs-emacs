;;; jcs-hook.el --- All the hook event do here.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jcs-focus-in-hook ()
  "When window is focus."
  (jcs-revert-all-file-buffers)
  (jcs--lsp-ui-doc-show-safely))
(add-hook 'focus-in-hook 'jcs-focus-in-hook)

(defun jcs-focus-out-hook ()
  "When window is not focus."
  (jcs--lsp-ui-doc-stop-timer)
  (jcs--lsp-ui-doc--hide-frame))
(add-hook 'focus-out-hook 'jcs-focus-out-hook)

(defvar jcs--lsp-lv-was-alive nil
  "Record ` *LV*' buffer was alive.")

(defvar jcs--lsp-lv-recording nil
  "Check if we are recording ")

(defun jcs-window-size-change-functions (&rest _)
  "When window changed size."
  (when (and (boundp 'lsp-mode) lsp-mode)
    (if (jcs--lsp-lv-buffer-alive-p)
        (setq jcs--lsp-lv-was-alive t)
      (if jcs--lsp-lv-was-alive
          (progn
            (when (jcs--lsp-current-last-signature-buffer)
              (let ((pt (point)))
                (jcs-window-restore-once)
                (goto-char pt)))
            (setq jcs--lsp-lv-was-alive nil))
        (let ((jcs--lsp-lv-recording t)) (jcs-window-record-once)))))
  (jcs--lsp-ui-doc-show-safely))
(add-hook 'window-size-change-functions 'jcs-window-size-change-functions)

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-find-file-hook ()
  "Find file hook."
  (unless (jcs-reload-emacs-reloading-p)
    (when (and (jcs-is-contain-list-string jcs-find-file-read-only-paths
                                           (buffer-file-name))
               (not jcs-package-installing))
      (read-only-mode 1))
    (jcs-active-line-numbers-by-mode)))
(add-hook 'find-file-hook 'jcs-find-file-hook)

(defun jcs--find-file--advice-before (&rest _args)
  "Advice execute before `find-file' command."
  ;; Fixed `css-mode' opening virtual buffer with directory error. You just
  ;; need to preload this before actually create the virtual buffer.
  (ignore-errors (require 'eww)))
(advice-add 'find-file :before #'jcs--find-file--advice-before)

(defun jcs--find-file--advice-after (&rest _args)
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

(defun jcs--switch-to-buffer--advice-after (&rest _args)
  "Advice execute after `switch-to-buffer' command."
  (jcs--neotree-start-refresh)
  (jcs-buffer-menu-safe-refresh))
(advice-add 'switch-to-buffer :after #'jcs--switch-to-buffer--advice-after)

(defun jcs--other-window--advice-before (&rest _args)
  "Advice execute before `other-window' command."
  (unless jcs--no-advice-other-window
    (jcs--lsp-ui-doc-stop-timer)
    (jcs--lsp-ui-doc--hide-frame)))
(advice-add 'other-window :before #'jcs--other-window--advice-before)

(defun jcs--other-window--advice-after (&rest _args)
  "Advice execute after `other-window' command."
  (unless jcs--no-advice-other-window
    ;;--------------------------------------------------------------------
    ;; TODO: This occurs in version `27.0.91'.
    ;; Maybe we don't need this anymore after `27.1' is released!
    (when (jcs-frame-util-p)
      (cl-case this-command
        ('jcs-other-window-next (jcs-other-window-next))
        ('jcs-other-window-prev (jcs-other-window-prev))))
    ;;--------------------------------------------------------------------
    (select-frame-set-input-focus (selected-frame))
    (jcs--neotree-start-refresh)
    (when (and (boundp 'neo-buffer-name)
               (not (string= neo-buffer-name (buffer-name (current-buffer)))))
      (setq jcs--neotree--last-window (selected-window)))
    (jcs-buffer-menu-safe-refresh)
    (jcs-dashboard-safe-refresh-buffer)
    (jcs--lsp-signature-maybe-stop)
    (jcs--lsp-ui-doc-show-safely)))
(advice-add 'other-window :after #'jcs--other-window--advice-after)

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-after-init-hook ()
  "Hook run after initialize."
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;; NOTE: Load required packages here.
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (progn
    (require 'alt-codes)
    (require 'auto-highlight-symbol)
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
  (jcs-gc-cons-threshold nil)
  (setq file-name-handler-alist jcs-file-name-handler-alist)

  ;; IMPORTANT: This should always be the last thing.
  (setq dashboard-init-info
        (format "[ %s ] [ Total took %0.1f seconds ]"
                dashboard-init-info
                (string-to-number (emacs-init-time)))))
(add-hook 'after-init-hook 'jcs-after-init-hook)

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-pre-command-hook ()
  "Hook run before every command."
  (when jcs--marking-whole-buffer-p
    (unless (= jcs--marking-whole-buffer--curosr-pos (point))
      (deactivate-mark)
      (setq jcs--marking-whole-buffer--curosr-pos -1)
      (setq jcs--marking-whole-buffer-p nil))))
(add-hook 'pre-command-hook 'jcs-pre-command-hook)

(defun jcs-post-command-hook ()
  "Hook run after every command."
  (cond
   ((jcs-is-current-major-mode-p "json-mode")
    (js2-minor-mode-exit))
   ((jcs-is-current-major-mode-p "web-mode")
    (when jcs-web-auto-truncate-lines
      (jcs-web-truncate-lines-by-face))))

  (jcs--lsp-ui-doc--hide-frame)
  (jcs--lsp-ui-doc-show-safely)
  (jcs-reload-active-mode-with-error-handle)
  (jcs-feebleline-display-mode-line-graphic))
(add-hook 'post-command-hook 'jcs-post-command-hook)

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-after-change-major-mode-hook ()
  "Hook run after major mode changes."
  (unless (jcs-reload-emacs-reloading-p) (jcs-active-line-numbers-by-mode)))
(add-hook 'after-change-major-mode-hook 'jcs-after-change-major-mode-hook)

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs--kill-emacs-hook ()
  "Hook run before Emacs is killed."
  (when (and (boundp 'ffmpeg-player-clean) (functionp 'ffmpeg-player-clean))
    (ffmpeg-player-clean)))
(add-hook 'kill-emacs-hook 'jcs--kill-emacs-hook)

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar jcs-emacs-ready-p nil
  "Flag to check if Emacs is ready.")

(defun jcs--emacs-startup-hook ()
  "Hook run after Emacs is startup."
  (with-current-buffer "*scratch*" (setq jcs-scratch--content (buffer-string)))
  (setq jcs-emacs-ready-p t))
(add-hook 'emacs-startup-hook 'jcs--emacs-startup-hook)

;;----------------------------------------------------------------------------
;; Minibuffer

(defun jcs-minibuffer-setup-hook ()
  "Hook when minibuffer setup."
  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (progn
    (jcs-gc-cons-threshold t))

  (jcs-dark-blue-mode-line)

  ;; Register hook.
  (add-hook 'post-command-hook #'jcs-minibuffer-post-command-hook nil t))
(add-hook 'minibuffer-setup-hook 'jcs-minibuffer-setup-hook)

(defun jcs-minibuffer-post-command-hook ()
  "Minibuffer post command hook."
  (when ivy-mode
    (cond ((jcs-is-finding-file-p)
           (when (and (save-excursion (search-backward "~//" nil t))
                      (not (jcs-current-char-equal-p "/")))
             (save-excursion
               (forward-char -1)
               (backward-delete-char 1)))))))

(defun jcs-minibuffer-exit-hook ()
  "Hook when exit minibuffer."
  (jcs-reload-active-mode)

  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (progn
    (garbage-collect)
    (jcs-gc-cons-threshold nil)))
(add-hook 'minibuffer-exit-hook 'jcs-minibuffer-exit-hook)

(provide 'jcs-hook)
;;; jcs-hook.el ends here
