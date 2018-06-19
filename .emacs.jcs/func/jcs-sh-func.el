;; ========================================================================
;; $File: jcs-sh-func.el $
;; $Date: 2018-06-19 20:58:38 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


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
