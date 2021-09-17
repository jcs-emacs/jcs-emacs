;;; jcs-revbuf.el --- Revert buffer module  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jcs-savbuf)

(defun jcs-revert-buffer-p (buf type)
  "Return non-nil if the BUF can be revert.

Argument TYPE can either be the following value.

  * list - List of buffer name you would want to revert for virtual buffer.
  * boolean - If it's non-nil, revert all virtual buffers."
  (cond ((listp type)
         (jcs-contain-list-string-regexp type (buffer-name buf)))
        (t type)))

(defun jcs-revert-all-virtual-buffers (type &optional clean-lr)
  "Revert all virtual buffers.

Argument TYPE see function `jcs-revert-buffer-p' description.

Argument CLEAN-LR see function `jcs-revert-buffer-no-confirm' description."
  (let ((buf-lst (jcs-virtual-buffer-list)))
    (dolist (buf buf-lst)
      (when (and (buffer-name buf) (jcs-revert-buffer-p buf type))
        (with-current-buffer buf (jcs-revert-buffer-no-confirm clean-lr))))))

(defun jcs-revert-all-valid-invalid-buffers (buf-lst type &optional clean-lr)
  "Revert all valid buffers.

Argument TYPE see function `jcs-revert-buffer-p' description.

Argument CLEAN-LR see function `jcs-revert-buffer-no-confirm' description."
  (let (filename normal-buffer-p do-revert-p)
    (dolist (buf buf-lst)
      (setq filename (buffer-file-name buf)
            normal-buffer-p (and filename
                                 (not (buffer-modified-p buf))
                                 (not (jcs-is-current-file-empty-p buf))))
      (when normal-buffer-p
        (if (file-readable-p filename) (setq do-revert-p t)
          (let (kill-buffer-query-functions) (kill-buffer buf))))
      (when (and (buffer-name buf) (or (jcs-revert-buffer-p buf type) do-revert-p))
        (with-current-buffer buf (jcs-revert-buffer-no-confirm clean-lr))))))

(defun jcs-revert-all-virtual-buffers--internal ()
  "Internal function to revert all vritual buffers."
  (jcs-save-window-excursion
    (save-window-excursion
      (jcs-revert-all-virtual-buffers jcs-revert-default-buffers))))

(defun jcs-revert-all-valid-buffers--internal ()
  "Internal function to revert all valid buffers."
  (save-window-excursion
    (jcs-revert-all-valid-invalid-buffers (jcs-valid-buffer-list) nil)))

(defun jcs-revert-all-invalid-buffers--internal ()
  "Internal function to revert all valid buffers."
  (save-window-excursion
    (jcs-revert-all-valid-invalid-buffers (jcs-invalid-buffer-list) nil)))

(defun jcs-ask-revert-all (bufs &optional index)
  "Ask to revert all buffers decided by ANSWER.

This is called when only buffer changes externally and there are modification
still in this editor.

Optional argument INDEX is used to loop through BUFS."
  (require 's)
  (unless index (setq index 0))
  (let* ((buf (nth index bufs)) path prompt answer)
    (when buf
      (setq path (buffer-file-name buf)
            prompt (concat
                    path "\n
The file has unsaved changes inside this editor and has been changed externally.
Do you want to reload it and lose the changes made in this source editor? ")
            answer (completing-read prompt '("Yes" "Yes to All" "No" "No to All"))
            index (1+ index))
      (pcase answer
        ("Yes"
         (with-current-buffer buf (jcs-revert-buffer-no-confirm t))
         (jcs-ask-revert-all bufs index))
        ("Yes to All"
         (jcs-revert-all-valid-buffers--internal)
         (jcs-revert-all-invalid-buffers--internal))
        ("No"
         (jcs-ask-revert-all bufs index))
        ;; Does nothing, exit.
        ("No to All")))))

(defun jcs-buffer-edit-externally-p (&optional buf)
  "Return non-nil if BUF is edited externally."
  (unless buf (setq buf (current-buffer)))
  (let* ((path (buffer-file-name buf))
         (buffer-saved-md5 (with-current-buffer buf jcs-buffer-save-string-md5))
         (file-content (jcs-get-string-from-file path))
         (file-content-md5 (md5 file-content)))
    (not (string= file-content-md5 buffer-saved-md5))))

(defun jcs-un-save-buffer-edit-externally-p (&optional buf)
  "Return non-nil if BUF is edit externally and is unsaved.
This function is used to check for lose changes from source editor."
  (unless buf (setq buf (current-buffer)))
  (and (buffer-modified-p buf) (jcs-buffer-edit-externally-p buf)))

(defun jcs-un-save-modified-buffers ()
  "Return non-nil if there is un-save modified buffer."
  (let ((buf-lst (jcs-valid-buffer-list)) un-save-buf-lst)
    (dolist (buf buf-lst)
      (when (jcs-un-save-buffer-edit-externally-p (get-buffer buf)) (push buf un-save-buf-lst)))
    (reverse un-save-buf-lst)))

(provide 'jcs-revbuf)
;;; jcs-revbuf.el ends here
