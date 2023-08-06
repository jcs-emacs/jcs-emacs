;;; lang/scheme/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-scheme-template "scheme" "default.txt"
  "Header for Scheme header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'scheme-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]ss")
                              'jcs-insert-scheme-template))
