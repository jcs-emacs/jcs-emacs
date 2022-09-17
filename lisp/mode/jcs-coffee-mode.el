;;; jcs-coffee-mode.el --- CoffeeScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'coffee-mode)

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

(provide 'jcs-coffee-mode)
;;; jcs-coffee-mode.el ends here
