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
    (jcs-safe-revert-all-buffers)))

(defun jcs-hook--focus-out ()
  "When window is not focus."
  )

(add-function
 :after after-focus-change-function
 (lambda () (if (frame-focus-state) (jcs-hook--focus-in) (jcs-hook--focus-out))))

(jcs-add-hook 'window-size-change-functions
  (jcs-dashboard--window-size-change)
  (when (featurep 'jcs-ivy) (jcs-ivy--window-size-change))
  (when (featurep 'treemacs) (jcs-treemacs--window-size-change)))

;;
;; (@* "Find Files" )
;;

(jcs-add-hook 'find-file-hook
  (jcs-funcall-fboundp #'jcs-update-buffer-save-string)
  (jcs-active-line-numbers-by-mode)
  (jcs-project-remember)
  (jcs-project--track-open-projects))

(jcs-advice-add 'find-file :after
  (when jcs-current-created-parent-dir-path
    (setq jcs-created-parent-dir-path jcs-current-created-parent-dir-path
          jcs-current-created-parent-dir-path nil))
  (jcs-buffer-menu-safe-refresh)
  (jcs-dashboard-safe-refresh-buffer))

(jcs-advice-add 'switch-to-buffer :after
  (jcs-buffer-menu-safe-refresh)
  (jcs-dashboard-safe-refresh-buffer))

(defun jcs-hook--other-window-interactively-p ()
  "Return non-nil, if executing `other-window'."
  (memq this-command '(other-window jcs-other-window-prev jcs-other-window-next)))

(jcs-advice-add 'other-window :before
  (when (jcs-hook--other-window-interactively-p)
    (jcs-funcall-fboundp #'company-abort)))

(defun jcs--other-window--advice-after (count &rest _)
  "Advice execute after command `other-window'."
  (cond ((jcs-frame-util-p)  ; skip if util
         (other-window (if (> count 0) 1 -1) t))
        ((jcs-hook--other-window-interactively-p)
         (select-frame-set-input-focus (selected-frame))
         (jcs-buffer-menu-safe-refresh)
         (jcs-dashboard-safe-refresh-buffer))))
(advice-add 'other-window :after #'jcs--other-window--advice-after)

;;
;; (@* "Initialization" )
;;

(jcs-add-hook 'after-init-hook
  (jcs-require '(dashboard diminish))
  (powerline-default-theme)
  (use-ttf-set-default-font)

  (run-with-idle-timer 0 nil #'jcs-hook--init-delay)

  (jcs-setup-default-theme)
  (jcs-depend-mode)

  ;; Frame Title
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

  ;; Lower the `GC' back to normal threshold
  (jcs-gc-cons-threshold-speed-up nil)
  (setq file-name-handler-alist jcs-file-name-handler-alist)

  ;; This should always be the last thing
  (jcs-dashboard-init-info))

(defun jcs-hook--init-delay ()
  "Delay some executions for faster speed."
  (jcs-with-gc-speed-up
    (jcs-mode-load-requires)
    (jcs-require '(jcs-edit jcs-comment jcs-vs jcs-nav))
    (global-alt-codes-mode 1)
    (global-auto-highlight-symbol-mode t)
    (auto-read-only-mode 1)
    (balanced-windows-mode 1)
    (global-company-mode t)
    (delete-selection-mode 1)
    (global-docstr-mode 1)
    (global-hl-line-mode 1)
    (global-hl-todo-mode 1)
    (indent-control-mode 1)
    (ivy-mode 1)
    (global-page-break-lines-mode 1)
    (global-region-occurrences-highlighter-mode 1)
    (right-click-context-mode 1)
    (show-paren-mode t)
    (global-so-long-mode 1)
    (transient-mark-mode t)
    (global-tree-sitter-mode 1)
    (which-key-mode)
    (global-yascroll-bar-mode 1)
    (with-current-buffer jcs-message-buffer-name (messages-buffer-mode)))
  (message nil))  ; mute at the very end!

;;
;; (@* "Pre/Post Command" )
;;

(jcs-add-hook 'pre-command-hook
  (jcs--er/record-history))

(jcs-add-hook 'post-command-hook
  (jcs--er/resolve-region)
  (jcs-funcall-fboundp #'jcs--mark-whole-buffer-resolve)
  (jcs-reload-active-mode-with-error-handle))

;;
;; (@* "Major Mode" )
;;

(jcs-add-hook 'after-change-major-mode-hook
  (jcs-active-line-numbers-by-mode))

;;
;; (@* "Quitting" )
;;

(defun jcs--quit-command (&rest _)
  "Advice for quit command."
  (deactivate-mark)
  (jcs-funcall-fboundp #'jcs-process-reporter-done))

(advice-add 'keyboard-quit :before #'jcs--quit-command)
(advice-add 'top-level :before #'jcs--quit-command)

;;
;; (@* "Startup" )
;;

(defvar jcs-emacs-startup-directory nil
  "Record the startup directory.")

(jcs-add-hook 'emacs-startup-hook
  (with-current-buffer jcs-scratch-buffer-name
    (setq jcs-scratch--content (buffer-string)))
  (setq jcs-emacs-startup-directory default-directory))

(provide 'jcs-hook)
;;; jcs-hook.el ends here
