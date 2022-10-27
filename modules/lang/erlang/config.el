;;; lang/erlang/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-erlang-template "erlang" "default.txt"
  "Template for Erlang Lisp.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'erlang-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]erl"
                                "[.]hrl")
                              'jcs-insert-erlang-template))
