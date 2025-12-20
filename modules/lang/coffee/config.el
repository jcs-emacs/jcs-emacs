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
  (company-fuzzy-backend-add-before 'company-coffee 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]coffee")
                              'jcs-insert-coffee-template))

;;
;; (@* "Extensions" )
;;

(use-package flymake-coffee
  :hook (coffee-mode . flymake-coffee-load))
