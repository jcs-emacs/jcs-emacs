;;; jcs-hook.el --- All the hook event do here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Focus In/Out" )
;;

(defun jcs-hook--focus-in ()
  "When window is focus."
  (jcs-funcall-fboundp #'jcs-revert-all-buffers)
  (jcs-dashboard-refresh-buffer))

(defun jcs-hook--focus-out ()
  "When window is not focus."
  )

(add-function
 :after after-focus-change-function
 (lambda () (if (frame-focus-state) (jcs-hook--focus-in) (jcs-hook--focus-out))))

;;
;; (@* "Find Files" )
;;

(jcs-add-hook 'find-file-hook
  (jcs-line-numbers-active-by-mode)
  (jcs-project-remember)
  (jcs-project--track-open-projects))

(jcs-advice-add 'find-file :after
  (when jcs-current-created-parent-dir-path
    (setq jcs-created-parent-dir-path jcs-current-created-parent-dir-path
          jcs-current-created-parent-dir-path nil)))

(jcs-add-hook 'window-state-change-hook
  (unless (active-minibuffer-window)
    (jcs-buffer-menu-refresh-buffer)
    (jcs-dashboard-refresh-buffer)))

;;
;; (@* "Initialization" )
;;

(jcs-add-hook 'after-init-hook
  (jcs-require '(dashboard moody))
  (use-ttf-set-default-font)

  (run-with-idle-timer 0 nil #'jcs-hook--init-delay)

  (jcs-setup-default-theme)
  (jcs-depend-mode))

(defun jcs-hook--init-delay ()
  "Delay some executions for faster speed."
  (global-auto-highlight-symbol-mode t)
  (auto-read-only-mode 1)
  (balanced-windows-mode 1)
  (global-company-mode t)
  (delete-selection-mode 1)
  (global-docstr-mode 1)
  (global-hl-line-mode 1)
  (global-hl-todo-mode 1)
  (indent-control-mode 1)
  (marginalia-mode 1)
  (message-clean-mode 1)
  (minions-mode 1)
  (mode-icons-mode 1)
  (global-page-break-lines-mode 1)
  (global-region-occurrences-highlighter-mode 1)
  (right-click-context-mode 1)
  (show-paren-mode t)
  (global-so-long-mode 1)
  (transient-mark-mode t)
  (global-tree-sitter-mode 1)
  (vertico-mode 1)
  (which-key-mode 1)
  (global-whitespace-cleanup-mode 1)
  (global-yascroll-bar-mode 1)
  (jcs-funcall-fboundp #'jcs-mode-load-requires)
  (jcs-require '(jcs-minibuf jcs-edit jcs-vs))
  (jcs-with-current-buffer jcs-message-buffer-name (messages-buffer-mode))
  (jcs-with-current-buffer jcs-scratch-buffer-name (lisp-interaction-mode))
  ;; Lower the `GC' back to normal threshold
  (jcs-gc-cons-threshold-speed-up nil)
  (setq file-name-handler-alist jcs-file-name-handler-alist)
  (message nil))  ; mute at the very end!

;;
;; (@* "Pre/Post Command" )
;;

(jcs-add-hook 'pre-command-hook
  (jcs--er/record-history))

(jcs-add-hook 'post-command-hook
  (jcs--er/resolve-region)
  (jcs-reload-active-mode-with-error-handle))

;;
;; (@* "Major Mode" )
;;

(jcs-add-hook 'after-change-major-mode-hook
  (jcs-line-numbers-active-by-mode))

;;
;; (@* "Quitting" )
;;

(jcs-advice-add '(keyboard-quit top-level) :before
  (deactivate-mark)  ; disable region
  (jcs-funcall-fboundp #'jcs-process-reporter-done)
  (jcs-backtrace-exit))

;;
;; (@* "Startup" )
;;

(defvar jcs-emacs-startup-directory nil
  "Record the startup directory.")

(jcs-add-hook 'emacs-startup-hook
  (setq jcs-emacs-startup-directory default-directory))

(provide 'jcs-hook)
;;; jcs-hook.el ends here
