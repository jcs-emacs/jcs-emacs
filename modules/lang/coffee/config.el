;;; lang/coffee/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-coffee-template "coffee" "default.txt"
  "Template for CoffeeScript.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'coffee-mode-hook
  (company-fuzzy-backend-add 'company-coffee)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]coffee")
                              'jcs-insert-coffee-template))
