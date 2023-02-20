;;; lang/agda/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-agda-template "agda" "default.txt"
  "Template for Agda.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'agda2-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]agda" "[.]lagda")
                              'jcs-insert-agda-template))
