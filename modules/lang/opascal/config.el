;;; lang/opascal/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-opascal-template "opascal" "default.txt"
  "Header for Object Pascal header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'opascal-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]dpk"
                                "[.]dpr")
                              'jcs-insert-opascal-template))
