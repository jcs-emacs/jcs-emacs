;;; lang/idris/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-idris-template "idris" "default.txt"
  "Idris file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'idris-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]lidr")
                              'jcs-insert-idris-template))
