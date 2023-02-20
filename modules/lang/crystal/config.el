;;; lang/crystal/config.el  -*- lexical-binding: t; -*-

(require 'flycheck-crystal)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-crystal-template "crystal" "default.txt"
  "Template for Crystal.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'crystal-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]cr")
                              'jcs-insert-crystal-template))
