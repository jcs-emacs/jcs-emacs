;;; jcs-function.el --- Self defines function  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Config" )
;;

(defun jcs-update-config ()
  "Update JCS-Emacs configurations to the latest version."
  (interactive)
  (let* ((dir (expand-file-name user-emacs-directory))
         (git-dir (f-directories user-emacs-directory
                                 (lambda (file)
                                   (and (file-directory-p file)
                                        (string= ".git" (file-name-nondirectory file)))))))
    (unless (file-exists-p dir)
      (user-error "[ERROR] User directory doesn't exist: %s" dir))
    (unless git-dir
      (user-error "[WARNING] Not a git directory: %s" user-emacs-directory))
    (msgu-inhibit-log
      (message "[INFO] Updating configurations... ")
      (cd dir)
      (shell-command "git pull")
      (message "[INFO] Updating configurations... done"))))

;;
;; (@* "*Messages*" )
;;

(defun jcs-messages ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (switch-to-buffer (messages-buffer)))

(defun jcs-messages-other-window ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window (messages-buffer)))

(defun jcs-messages-maybe-kill-this-buffer ()
  "Erase the *Messages* buffer."
  (interactive)
  ;; Message one message to retrieve `*Message*' buffer prepare for next use.
  ;; Or else it some operation might prompt some issue that needed `*Message*'
  ;; buffer to be exists.
  (when (jcs-maybe-kill-this-buffer)
    (message ".") (jcs-messages-erase-buffer)))

(defun jcs-messages-erase-buffer ()
  "Reopen *Messages* buffer."
  (interactive)
  (with-current-buffer (messages-buffer)
    (let (buffer-read-only)
      (erase-buffer)
      (insert (format "Retrieving %s buffer..\n" (buffer-name)))
      (message nil))))  ; clear echo area

;;
;; (@* "*scratch*" )
;;

(defun jcs-scratch-buffer-p ()
  "Return non-nil if current buffer the scratch buffer."
  (equal (current-buffer) (get-scratch-buffer-create)))

(defun jcs-scratch-other-window ()
  "Start a new scratch buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window (get-scratch-buffer-create)))

(defun jcs-new-scratch-buffer ()
  "Start a new scratch buffer."
  (interactive)
  (scratch-buffer)
  (erase-buffer)
  (ignore-errors (insert (substitute-command-keys initial-scratch-message)))
  (goto-char (point-min))
  (lisp-interaction-mode))

(defun jcs-scratch-buffer-refresh ()
  "Refresh scratch buffer."
  (interactive)
  (if (jcs-scratch-buffer-p) (jcs-new-scratch-buffer) (jcs-reopen-this-buffer)))

;;
;; (@* "LSP" )
;;

(defun jcs-lsp-connected-p ()
  "Return non-nil if LSP connected."
  (bound-and-true-p lsp-managed-mode))

(defun jcs-lsp-safe-active ()
  "Safe way to active LSP."
  (unless (jcs-lsp-connected-p) (lsp-deferred)))

(defun jcs-lsp-maybe-shutdown ()
  "Maybe shutdown the workspace."
  (when-let* ((workspaces (jcs-fboundp-apply #'lsp-workspaces)))
    (dolist (workspace workspaces)
      (let* ((buffers (lsp--workspace-buffers workspace))
             (buffers (cl-remove-if-not #'buffer-live-p buffers)))
        (when (<= (length buffers) 1)
          (lsp-workspace-shutdown workspace))))))

;;
;; (@* "Magit" )
;;

(defun jcs-magit ()
  "Start magit."
  (interactive)
  (if-let* ((buf (magit-get-mode-buffer 'magit-status-mode)))
      (magit-switch-to-repository-buffer-other-window buf)
    (call-interactively #'magit)))

(provide 'jcs-function)
;;; jcs-function.el ends here
