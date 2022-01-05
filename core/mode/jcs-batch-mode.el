;;; jcs-batch-mode.el --- Batch mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'bat-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-batch-template ()
  "Header format for batch file."
  (jcs--file-header--insert "batch" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'bat-mode-hook
  (setq comment-start "::")

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]bat")
                              'jcs-insert-batch-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(provide 'jcs-batch-mode)
;;; jcs-batch-mode.el ends here
