;;; jcs-feebleline-func.el --- Feebleline function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defface jcs--feebleline-read-only--enabled
  '((t (:foreground "#FF0000")))
  "Ready only symbol face when active.."
  :group 'jcs)
(defvar jcs--feebleline-read-only--enabled 'jcs--feebleline-read-only--enabled)

(defface jcs--feebleline-read-only--disabled
  '((t (:foreground "#00FF00")))
  "Ready only symbol face when deactive."
  :group 'jcs)
(defvar jcs--feebleline-read-only--disabled 'jcs--feebleline-read-only--disabled)


(defun jcs--feebleline--symbol-read-only ()
  "Feebleline read-only symbol."
  (propertize " ≡" 'face jcs--feebleline-read-only--enabled)
  )

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
