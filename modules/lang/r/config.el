;;; lang/r/config.el  -*- lexical-binding: t; -*-

(require 'ess-r-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-r-template "r" "default.txt"
  "Header for R header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ess-r-mode-hook
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]r")
                              'jcs-insert-r-template))
