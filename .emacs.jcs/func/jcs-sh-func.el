;;; jcs-sh-func.el --- ShellScript related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun jcs-ask-line-endings-for-this-sh-script (type)
  "Ask the saved line endings TYPE for this shell script."
  (require 'show-eol)
  (interactive
   (list
    (completing-read
     (format "Line Endings Type for file `%s`: " (jcs-buffer-name-or-buffer-file-name))
     (let ((read-lst '("Windows (dos)" "macOS (mac)" "Linux (unix)")))
       (push (format "=> system: (%s)" (jcs-get-current-sysem)) read-lst)
       (push (format "=> file: (%s)" (show-eol--get-current-system)) read-lst)
       read-lst))))
  (let ((sys-type nil))
    (cond ((string= type "Windows (dos)") (setq sys-type 'dos))
          ((string= type "macOS (mac)") (setq sys-type 'mac))
          ((string= type "Linux (unix)") (setq sys-type 'unix))
          ((string-match-p "file" type) (setq sys-type (show-eol--get-current-system)))
          ((string-match-p "system" type) (setq sys-type (jcs-get-current-sysem))))
    (set-buffer-file-coding-system sys-type)))

;;;###autoload
(defun jcs-sh-untabify-save-buffer ()
  "ShellScript save buffer function."
  (interactive)
  (call-interactively #'jcs-ask-line-endings-for-this-sh-script)
  (jcs-untabify-save-buffer))

;;;###autoload
(defun jcs-sh-tabify-save-buffer ()
  "ShellScript save buffer function."
  (interactive)
  (call-interactively #'jcs-ask-line-endings-for-this-sh-script)
  (jcs-tabify-save-buffer))

(provide 'jcs-sh-func)
;;; jcs-sh-func.el ends here
