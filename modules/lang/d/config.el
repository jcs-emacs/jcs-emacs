;;; lang/d/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-d-template "d" "default.txt"
  "Template for D.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'd-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]d")
                              'jcs-insert-d-template))
