;;; jcs-feebleline-func.el --- Feebleline function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defface jcs--feebleline-read-only
  '((t (:foreground "#FFE13D")))
  "Ready only symbol."
  :group 'jcs)
(defvar jcs--feebleline-read-only 'jcs--feebleline-read-only)


(defun jcs--feebleline--symbol-read-only ()
  "Feebleline read-only symbol."
  (if buffer-read-only "¢" "δ"))

(defun jcs--feebleline--project-name ()
  "Feebleline project name."
  (let ((project-root (cdr (project-current))))
    (if (and project-root
             (buffer-file-name))
        (file-name-nondirectory (directory-file-name project-root))
      "¥")))

(defun jcs--feebleline--coding-system ()
  "Feebleline coding system."
  buffer-file-coding-system)

(defun jcs--feebleline--spc/tab ()
  "Feebleline spaces or tabs."
  (format "%s : %s" (jcs-buffer-spaces-to-tabs) (jcs-get-tab-width-by-mode)))

(defun jcs--feebleline--time ()
  "Feebleline time."
  (format-time-string "[%Y-%m-%d %H:%M:%S]"))


(provide 'jcs-feebleline-func)
;;; jcs-feebleline-func.el ends here
