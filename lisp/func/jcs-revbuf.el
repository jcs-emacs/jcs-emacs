;;; jcs-revbuf.el --- Revert buffer module  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom jcs-revbuf-clear-line-reminder nil
  "If non-nil, remove all sign from `line-reminder'."
  :type 'boolean
  :group 'jcs)

(defun jcs-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  ;; Record all the enabled mode that you want to remain enabled after
  ;; revert the file.
  (let ((was-flycheck (if (and (featurep 'flycheck) flycheck-mode) 1 -1))
        (was-readonly (if buffer-read-only 1 -1))
        (was-g-hl-line (if global-hl-line-mode 1 -1))
        (was-page-lines (if page-break-lines-mode 1 -1)))
    ;; Revert it!
    (ignore-errors (revert-buffer :ignore-auto :noconfirm :preserve-modes))
    (jcs-update-buffer-save-string)
    (when (and (featurep 'line-reminder) jcs-revbuf-clear-line-reminder)
      (line-reminder-clear-reminder-lines-sign))
    ;; Revert all the enabled mode.
    (flycheck-mode was-flycheck)
    (read-only-mode was-readonly)
    (global-hl-line-mode was-g-hl-line)
    (page-break-lines-mode was-page-lines)))

(defun jcs-revert-all-invalid-buffers ()
  "Revert all invalid buffers."
  (save-window-excursion
    (dolist (buf (jcs-invalid-buffer-list))
      (with-current-buffer buf
        (when jcs-buffer-save-string-md5  ; this present only after first save!
          (set-buffer-modified-p nil)
          (let (kill-buffer-query-functions) (kill-this-buffer)))))))

(defun jcs-revert-all-valid-buffers ()
  "Revert all valid buffers."
  (save-window-excursion
    (dolist (buf (jcs-valid-buffer-list))
      (with-current-buffer buf (jcs-revert-buffer-no-confirm)))))

(defun jcs-ask-revert-all (bufs &optional index)
  "Ask to revert all buffers decided by ANSWER.

This is called when only buffer changes externally and there are modification
still in this editor.

Optional argument INDEX is used to loop through BUFS."
  (when-let*
      ((index (or index 0)) (buf (nth index bufs))
       (path (buffer-file-name buf))
       (prompt (concat path "\n
The file has unsaved changes inside this editor and has been changed externally.
Do you want to reload it and lose the changes made in this source editor? "))
       (answer (completing-read prompt '("Yes" "Yes to All" "No" "No to All"))))
    (cl-incf index)
    (pcase answer
      ("Yes"
       (with-current-buffer buf (jcs-revert-buffer-no-confirm t))
       (jcs-ask-revert-all bufs index))
      ("Yes to All"
       (jcs-revert-all-valid-buffers)
       (jcs-revert-all-invalid-buffers))
      ("No" (jcs-ask-revert-all bufs index))
      ("No to All"))))  ; Does nothing, exit

(defun jcs-buffer-edit-externally-p (&optional buf)
  "Return non-nil if BUF is edited externally."
  (let* ((buf (or buf (current-buffer)))
         (path (buffer-file-name buf))
         (buffer-saved-md5 (with-current-buffer buf jcs-buffer-save-string-md5))
         (file-content (jcs-get-string-from-file path))
         (file-content-md5 (md5 file-content)))
    (not (string= file-content-md5 buffer-saved-md5))))

(defun jcs-un-save-buffer-edit-externally-p (&optional buf)
  "Return non-nil if BUF is edit externally and is unsaved.
This function is used to check for lose changes from source editor."
  (let ((buf (or buf (current-buffer))))
    (and (buffer-modified-p buf) (jcs-buffer-edit-externally-p buf))))

(defun jcs-un-save-modified-buffers ()
  "Return non-nil if there is un-save modified buffer."
  (let (un-save-buf-lst)
    (dolist (buf (jcs-valid-buffer-list))
      (when (jcs-un-save-buffer-edit-externally-p (get-buffer buf))
        (push buf un-save-buf-lst)))
    (reverse un-save-buf-lst)))

(provide 'jcs-revbuf)
;;; jcs-revbuf.el ends here
