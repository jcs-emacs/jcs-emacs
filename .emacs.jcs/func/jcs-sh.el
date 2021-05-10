;;; jcs-sh.el --- ShellScript related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar-local jcs-sh-buffer-eol nil
  "Record of buffer's line endings type.")

;;;###autoload
(defun jcs-ask-line-endings-for-this-sh-script (type)
  "Ask the saved line endings TYPE for this shell script."
  (interactive
   (list
    (completing-read
     (format "Line Endings Type for file `%s`: " (jcs-buffer-name-or-buffer-file-name))
     (let ((read-lst '("Windows (dos)" "macOS (mac)" "Linux (unix)")))
       (require 'show-eol)
       (push (format "=> system: (%s)" (jcs-get-current-sysem)) read-lst)
       (push (format "=> file: (%s)" (show-eol--get-current-system)) read-lst)
       read-lst))))
  (let (sys-type)
    (cond ((string-match-p "file" type)
           (setq sys-type (show-eol--get-current-system)))
          ((string-match-p "system" type)))
    (pcase type
      ("Windows (dos)" (setq sys-type 'dos))
      ("macOS (mac)" (setq sys-type 'mac))
      ("Linux (unix)" (setq sys-type 'unix)))
    (setq jcs-sh-buffer-eol sys-type)
    (set-buffer-file-coding-system sys-type)))

(defun jcs-sh-before-save ()
  "Run execution before saving."
  (if jcs-sh-buffer-eol (set-buffer-file-coding-system jcs-sh-buffer-eol)
    (call-interactively #'jcs-ask-line-endings-for-this-sh-script)))

;;;###autoload
(defun jcs-sh-untabify-save-buffer ()
  "ShellScript save buffer function."
  (interactive)
  (jcs-sh-before-save)
  (jcs-untabify-save-buffer))

;;;###autoload
(defun jcs-sh-tabify-save-buffer ()
  "ShellScript save buffer function."
  (interactive)
  (jcs-sh-before-save)
  (jcs-tabify-save-buffer))

(provide 'jcs-sh)
;;; jcs-sh.el ends here
