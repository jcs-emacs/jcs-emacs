;;; lang/noir/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-noir-template "noir" "default.txt"
  "Header for Noir header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'noir-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]nr")
                              'jcs-insert-noir-template))
