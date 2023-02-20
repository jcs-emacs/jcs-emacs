;;; lang/caml/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-ocaml-template "caml" "default.txt"
  "Template for OCaml.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'caml-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]ml" "[.]mli")
                              'jcs-insert-ocaml-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-ocaml
  :hook (flycheck-mode . flycheck-ocaml-setup))
