;;; lang/batch/config.el  -*- lexical-binding: t; -*-

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

  (company-fuzzy-backend-add-before 'company-cmd 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]bat")
                              'jcs-insert-batch-template))
