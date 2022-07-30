;;; jcs-caml-mode.el --- OCaml mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'caml)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-ocaml-template ()
  "Template for OCaml."
  (jcs--file-header--insert "caml" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'caml-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]ml" "[.]mli")
                              'jcs-insert-ocaml-template))

(provide 'jcs-caml-mode)
;;; jcs-caml-mode.el ends here
