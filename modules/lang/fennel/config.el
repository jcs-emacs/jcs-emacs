;;; lang/fennel/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-fennel-template "fennel" "default.txt"
  "Template for Fennel Lisp.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'fennel-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]fnl")
                              'jcs-insert-fennel-template))

;;
;; (@* "Extensions" )
;;

(use-package flymake-fennel
  :hook (fennel-mode . flymake-fennel-setup))
