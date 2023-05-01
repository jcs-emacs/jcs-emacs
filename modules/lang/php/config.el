;;; lang/php/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-php-template "php" "default.txt"
  "Template for PHP.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'php-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]php")
                              'jcs-insert-php-template))

;;
;; (@* "Extensions" )
;;

(use-package flymake-php :hook (flymake-mode . flymake-php-load))
