;;; lang/fountain/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-fountain-template "fountain" "default.txt"
  "Template for Fountain Lisp.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'fountain-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]fountain")
                              'jcs-insert-fountain-template))
