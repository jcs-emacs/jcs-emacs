;;; lang/less-css/config.el  -*- lexical-binding: t; -*-

(require 'css-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-less-template "less" "default.txt"
  "Header for LESS header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'less-css-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]less")
                              'jcs-insert-less-template))

;;
;; (@* "Extensions" )
;;

(use-package flymake-less
  :hook (less-css-mode . flymake-less-load))
