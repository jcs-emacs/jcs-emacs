;;; lang/latex/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-latex-template "latex" "default.txt"
  "LaTex file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'latex-mode-hook
  (company-fuzzy-backend-add-before 'company-bibtex 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]tex")
                              'jcs-insert-latex-template))
