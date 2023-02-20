;;; lang/cobol/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-cobol-template "cobol" "default.txt"
  "Template for COBOL.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'cobol-mode-hook
  (electric-pair-mode nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cbl")
                              'jcs-insert-cobol-template))
