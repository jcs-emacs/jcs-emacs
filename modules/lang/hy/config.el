;;; lang/hy/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-hy-template "hy" "default.txt"
  "Header for Hylang header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'hy-mode-hook
  (company-fuzzy-backend-add-before 'company-hy 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]hy")
                              'jcs-insert-hy-template))
