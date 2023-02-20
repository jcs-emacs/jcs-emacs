;;; lang/json/config.el  -*- lexical-binding: t; -*-

(require 'json-snatcher)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-json-template "json" "default.txt"
  "Header for JSON header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'json-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]json")
                              'jcs-insert-json-template))
