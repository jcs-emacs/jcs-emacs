;;; jcs-batch-mode.el --- Batch mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'bat-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-batch-template "batch" "default.txt"
  "Header format for batch file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'bat-mode-hook
  (setq comment-start "::")

  (modify-syntax-entry ?_ "w")

  (company-fuzzy-backend-add 'company-cmd)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]bat")
                              'jcs-insert-batch-template))

(provide 'jcs-batch-mode)
;;; jcs-batch-mode.el ends here
