;;; jcs-julia-mode.el --- Julia mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'julia-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-julia-template "julia" "default.txt"
  "Julia file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'julia-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]jl")
                              'jcs-insert-julia-template))

(provide 'jcs-julia-mode)
;;; jcs-julia-mode.el ends here
