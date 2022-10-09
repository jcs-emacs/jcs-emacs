;;; jcs-hook.el --- All the hook event do here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Windows" )
;;

(defun jcs-hook--focus-in ()
  "When window is focus."
  (jcs-reload-active-mode)
  (jcs-dashboard-refresh-buffer))

(defun jcs-hook--focus-out ()
  "When window is not focus."
  (jcs-reload-active-mode))

(add-function
 :after after-focus-change-function
 (lambda () (if (frame-focus-state) (jcs-hook--focus-in) (jcs-hook--focus-out))))

(jcs-add-hook 'window-state-change-hook
  (when (and (not (active-minibuffer-window))
             (not (jcs-funcall-fboundp #'company--active-p)))
    (jcs-buffer-menu-refresh-buffer)
    (jcs-dashboard-refresh-buffer)
    (when echo-bar-mode (jcs-funcall-fboundp #'echo-bar-update))))

;;
;; (@* "Find Files" )
;;

(jcs-add-hook 'find-file-hook
  (jcs-line-numbers-active-by-mode)
  (jcs-project-remember)
  (jcs-project--track-open-projects))

;;
;; (@* "Initialization" )
;;

(jcs-add-hook 'after-init-hook
  (jcs-require '(dashboard moody on))
  (use-ttf-set-default-font)
  (jcs-setup-default-theme))

(jcs-add-hook 'on-init-ui-hook
  (run-with-idle-timer 0 nil #'jcs-hook--init-delay))

(defun jcs-hook--init-delay ()
  "Delay some executions for faster speed."
  (auto-scroll-bar-mode 1)
  (balanced-windows-mode 1)
  (global-eldoc-mode 1)
  (echo-bar-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (global-hl-line-mode 1)
  (global-hl-todo-mode 1)
  (indent-control-mode 1)
  (marginalia-mode 1)
  (message-clean-mode 1)
  (minions-mode 1)
  (right-click-context-mode 1)
  (show-paren-mode t)
  (vertico-mode 1)
  (vs-revbuf-mode 1)
  (jcs-funcall-fboundp #'jcs-mode-load-requires)
  (jcs-require '(jcs-edit))
  (jcs-with-current-buffer (messages-buffer) (messages-buffer-mode))
  (jcs-with-current-buffer (get-scratch-buffer-create) (lisp-interaction-mode))
  ;; Lower the `GC' back to normal threshold
  (jcs-gc-cons-threshold-speed-up nil)
  (message nil))  ; mute at the very end!

(jcs-add-hook 'on-first-input-hook
  (global-company-mode t)
  (delete-selection-mode 1)
  (diminish-buffer-mode 1)
  (ff-guard-mode 1)
  (gcmh-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (global-page-break-lines-mode 1)
  (recentf-excl-mode 1)
  (global-region-occurrences-highlighter-mode 1)
  (transient-mark-mode t)
  (global-vsc-edit-mode 1)
  (which-key-mode 1)
  (global-whitespace-cleanup-mode 1)
  (whole-line-or-region-global-mode 1))

(jcs-add-hook 'on-first-file-hook
  (auto-read-only-mode 1)
  (global-tree-sitter-mode 1)
  (global-so-long-mode 1))

;;
;; (@* "Input" )
;;

(jcs-add-hook 'pre-command-hook
  (jcs--er/record-history))

(jcs-add-hook 'post-command-hook
  (jcs--er/resolve-region)
  (jcs-reload-active-mode))

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
