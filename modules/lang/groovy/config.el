;;; lang/groovy/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-groovy-template "groovy" "default.txt"
  "Header for Groovy header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'groovy-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]groovy"
                                "[.]gradle")
                              'jcs-insert-groovy-template))
