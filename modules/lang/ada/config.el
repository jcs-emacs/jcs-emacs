;;; lang/ada/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-ada-template "ada" "default.txt"
  "Template for Ada.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ada-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]ads")
                              'jcs-insert-ada-template))
