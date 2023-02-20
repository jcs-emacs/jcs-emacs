;;; lang/elixir/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-elixir-template "elixir" "default.txt"
  "Template for Elixir.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'elixir-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word


  ;; File Header
  (jcs-insert-header-if-valid '("[.]ex"
                                "[.]exs")
                              'jcs-insert-elixir-template))
