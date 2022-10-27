;;; lang/sass/config.el  -*- lexical-binding: t; -*-

(require 'css-mode)
(require 'ssass-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-sass-template "sass" "default.txt"
  "Header for SASS header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ssass-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]sass")
                              'jcs-insert-sass-template))
