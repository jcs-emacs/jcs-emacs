;;; lang/fsharp/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-fsharp-template "fsharp" "default.txt"
  "Header for F# header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'fsharp-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]fs")
                              'jcs-insert-fsharp-template))
