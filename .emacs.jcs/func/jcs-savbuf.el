;;; jcs-savbuf.el --- Undo/Redo module  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar-local jcs-buffer-save-string-md5 nil
  "Buffer string when buffer is saved; this value encrypted with md5 algorithm.
This variable is used to check if file are edited externally.")

(defun jcs-update-buffer-save-string ()
  "Update variable `jcs-buffer-save-string-md5' once."
  (setq jcs-buffer-save-string-md5 (md5 (buffer-string))))

(defun jcs--save-buffer--advice-before (&rest _)
  "Execution before function `save-buffer'."
  (jcs-funcall-fboundp 'company-abort))
(advice-add 'save-buffer :before #'jcs--save-buffer--advice-before)

(defun jcs--save-buffer--advice-after (&rest _)
  "Execution after function `save-buffer'."
  (require 'jcs-undo)
  (jcs-update-buffer-save-string)
  (jcs-undo-kill-this-buffer)
  (jcs-update-line-number-each-window))
(advice-add 'save-buffer :after #'jcs--save-buffer--advice-after)

(defun jcs--organize-save-buffer ()
  "Organize save buffer."
  (require 'cl-lib)
  (let (deactivate-mark truncate-lines)
    (when jcs-on-save-whitespace-cleanup-p
      (jcs-delete-trailing-whitespace-except-current-line))
    (when jcs-on-save-end-trailing-lines-cleanup-p
      (jcs-remove-trailing-lines-end-buffer))
    (cl-case jcs-on-save-tabify-type
      (tabify (jcs-tabify-buffer))
      (untabify (jcs-untabify-buffer))
      ('nil (progn ))  ; Do nothing here.
      (t (user-error "[WARNING] Unknown tabify type when on save: %s" jcs-on-save-tabify-type)))
    (when jcs-on-save-remove-control-M-p
      (jcs-mute-apply (jcs-remove-control-M)))
    (jcs--save-buffer-internal)))

(defun jcs--organize-save-buffer--do-valid ()
  "Same with `jcs--organize-save-buffer', but with validity check infront."
  (cond
   ((not (buffer-file-name))
    (user-error "[WARNING] Can't save with invalid filename: %s" (buffer-name)))
   (buffer-read-only
    (user-error "[WARNING] Can't save read-only file: %s" buffer-read-only))
   (t (jcs--organize-save-buffer))))

(defun jcs-save-buffer-default ()
  "Save buffer with the default configuration's settings."
  (interactive)
  (jcs--organize-save-buffer--do-valid))

(defun jcs-untabify-save-buffer ()
  "Untabify file and save the buffer."
  (interactive)
  (let ((jcs-on-save-tabify-type 'untabify)) (jcs--organize-save-buffer--do-valid)))

(defun jcs-tabify-save-buffer ()
  "Tabify file and save the buffer."
  (interactive)
  (let ((jcs-on-save-tabify-type 'tabify)) (jcs--organize-save-buffer--do-valid)))

(defun jcs-save-buffer ()
  "Save buffer wrapper."
  (interactive)
  (let (jcs-on-save-tabify-type
        jcs-on-save-whitespace-cleanup-p
        jcs-on-save-end-trailing-lines-cleanup-p)
    (jcs--organize-save-buffer--do-valid)))

(defun jcs--save-buffer-internal ()
  "Internal core functions for saving buffer."
  (setq jcs-created-parent-dir-path nil)
  (let ((jcs-walking-through-windows-p t)
        (modified (buffer-modified-p))
        (readable (file-readable-p (buffer-file-name)))
        (cur-frame (selected-frame)))
    ;; For some mode, broken save.
    (jcs-mute-apply (save-excursion (save-buffer)))
    (select-frame-set-input-focus cur-frame)  ; For multi frames.
    ;; If wasn't readable, try to active LSP once if LSP is available.
    (unless readable (jcs--safe-lsp-active))
    (if (or modified (not readable))
        (message "Wrote file %s" (buffer-file-name))
      (message "(No changes need to be saved)"))))

(defun jcs-save-all-buffers ()
  "Save all buffers currently opened."
  (interactive)
  (let ((saved-lst '()) (len -1) (info-str ""))
    (save-window-excursion
      (dolist (buf (buffer-list))
        (switch-to-buffer buf)
        (when (ignore-errors
                (jcs-mute-apply (call-interactively (key-binding (kbd "C-s")))))
          (push buf saved-lst)
          (message "Saved buffer '%s'" buf))))
    (setq len (length saved-lst))
    (setq info-str (mapconcat (lambda (buf) (format "`%s`" buf)) saved-lst ", "))
    (pcase len
      (0 (message "[INFO] (No buffers need to be saved)"))
      (1 (message "[INFO] %s buffer saved: %s" len info-str))
      (_ (message "[INFO] All %s buffers are saved: %s" len info-str)))))

(defun jcs-save-buffer-function ()
  "Return save buffer function by mode."
  (cond
   ((jcs-is-current-major-mode-p '("snippet-mode"))
    #'jcs-save-buffer)
   ((jcs-is-current-major-mode-p '("java-mode"))
    #'jcs-java-untabify-save-buffer)
   ((jcs-is-current-major-mode-p '("cmake-mode"
                                   "makefile-mode"))
    #'jcs-tabify-save-buffer)
   ((jcs-is-current-major-mode-p '("sh-mode"))
    #'jcs-sh-untabify-save-buffer)
   ((jcs-is-current-major-mode-p '("conf-javaprop-mode"
                                   "ini-mode"
                                   "org-mode"
                                   "view-mode"))
    #'save-buffer)
   ((jcs-is-current-major-mode-p '("scss-mode"
                                   "ini-mode"))
    #'jcs-css-save-buffer)
   (t #'jcs-save-buffer-default)))

(provide 'jcs-savbuf)
;;; jcs-savbuf.el ends here
