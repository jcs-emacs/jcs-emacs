;;; jcs-julia-mode.el --- Julia mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'julia-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-julia-template ()
  "Julia file header format."
  (jcs--file-header--insert "julia" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'julia-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]jl")
                              'jcs-insert-julia-template))

(provide 'jcs-julia-mode)
;;; jcs-julia-mode.el ends here
