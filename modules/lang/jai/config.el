;;; lang/jai/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-jai-template "jai" "default.txt"
  "Header for JAI header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'jai-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]jai")
                              'jcs-insert-jai-template))
