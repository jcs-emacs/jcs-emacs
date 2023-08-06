;;; lang/fstar/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-fstar-template "fstar" "default.txt"
  "Header for F* header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'fstar-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]fsti")
                              'jcs-insert-fstar-template))
