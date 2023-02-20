;;; lang/julia/config.el  -*- lexical-binding: t; -*-

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
