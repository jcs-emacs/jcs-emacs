;;; lang/purescript/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-purescript-template "purescript" "default.txt"
  "Header for PureScript file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'purescript-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]purs")
                              'jcs-insert-purescript-template))
