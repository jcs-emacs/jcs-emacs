;;; lang/sml/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-sml-template "sml" "default.txt"
  "SML file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'sml-mode-hook
  (company-fuzzy-backend-add-before 'company-mlton-keyword 'company-dabbrev)
  (company-fuzzy-backend-add-before 'company-mlton-basis   'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sml")
                              'jcs-insert-sml-template))
