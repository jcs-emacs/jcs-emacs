;;; lang/haml/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-haml-template "haml" "default.txt"
  "Template for HAML.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'haml-mode-hook
  (jcs-insert-header-if-valid '("[.]haml")
                              'jcs-insert-haml-template))

;;
;; (@* "Extensions" )
;;

(use-package flymake-haml
  :hook (haml-mode . flymake-haml-load))
