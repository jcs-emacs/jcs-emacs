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
                              'jcs-insert-ocaml-template)

  (company-fuzzy-backend-add-before 'merlin-company-backend 'company-dabbrev))

;;
;; (@* "Extensions" )
;;

(use-package merlin-eldoc
  :hook (caml-mode . merlin-eldoc-setup))

(use-package flycheck-ocaml
  :hook (flycheck-mode . flycheck-ocaml-setup))
