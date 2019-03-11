;;; jcs-sh-func.el --- ShellScript related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-sh-untabify-save-buffer ()
  "ShellScript save buffer function."
  (interactive)
  (set-buffer-file-coding-system 'unix)
  (jcs-untabify-save-buffer))

;;;###autoload
(defun jcs-sh-tabify-save-buffer ()
  "ShellScript save buffer function."
  (interactive)
  (set-buffer-file-coding-system 'unix)
  (jcs-tabify-save-buffer))


(provide 'jcs-sh-func)
;;; jcs-sh-func.el ends here
