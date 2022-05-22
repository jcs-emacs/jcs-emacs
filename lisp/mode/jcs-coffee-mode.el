;;; jcs-coffee-mode.el --- CoffeeScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'coffee-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-coffee-template ()
  "Template for CoffeeScript."
  (jcs--file-header--insert "coffee" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'coffee-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]coffee")
                              'jcs-insert-coffee-template))

(provide 'jcs-coffee-mode)
;;; jcs-coffee-mode.el ends here
