;;; jcs-savbuf.el --- Save buffer module  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jcs-undo)

(defvar-local jcs-buffer-save-string-md5 nil
  "Buffer string when buffer is saved; this value encrypted with md5 algorithm.
This variable is used to check if file are edited externally.")

(defun jcs-update-buffer-save-string ()
  "Update variable `jcs-buffer-save-string-md5' once."
  (setq jcs-buffer-save-string-md5 (md5 (buffer-string))))

(jcs-advice-add 'save-buffer :before
  (jcs-funcall-fboundp 'company-abort))

(jcs-advice-add 'save-buffer :after
  (jcs-update-buffer-save-string)
  (undo-tree-kill-visualizer)
  (jcs-line-number-update-each-window))

(defun jcs-save-buffer--internal ()
  "Internal core functions for saving buffer."
  (setq jcs-created-parent-dir-path nil)
  (let ((modified (buffer-modified-p))
        (readable (file-readable-p (buffer-file-name)))
        (cur-frame (selected-frame)))
    ;; For some mode, broken save.
    (jcs-mute-apply (save-buffer))
    (select-frame-set-input-focus cur-frame)  ; For multi frames.
    ;; If wasn't readable, try to active LSP once if LSP is available.
    (unless readable (jcs--safe-lsp-active))
    (if (or modified (not readable))
        (message "Wrote file %s" (buffer-file-name))
      (message "(No changes need to be saved)"))))

(defun jcs-save-buffer--organize-before ()
  "Organize before save buffer."
  (let (deactivate-mark truncate-lines)
    ;; Delete trailing whitespaces execpt the current line
    (when whitespace-cleanup-mode
      (whitespace-cleanup-region (point-min) (line-beginning-position))
      (whitespace-cleanup-region (line-end-position) (point-max)))
    (when jcs-on-save-remove-control-M (jcs-mute-apply (jcs-remove-control-M)))
    (jcs-save-buffer--internal)))

(defun jcs-save-buffer ()
  "Save buffer wrapper."
  (interactive)
  (cond
   ((not (buffer-file-name))
    (user-error "[WARNING] Can't save with invalid filename: %s" (buffer-name)))
   (buffer-read-only
    (user-error "[WARNING] Can't save read-only file: %s" buffer-read-only))
   (t (jcs-save-buffer--organize-before))))

(defun jcs-save-buffer-function ()
  "Return save buffer function by mode."
  (cl-case major-mode
    ((or css-mode scss-mode) #'jcs-css-save-buffer)
    (t #'jcs-save-buffer)))

(provide 'jcs-savbuf)
;;; jcs-savbuf.el ends here
