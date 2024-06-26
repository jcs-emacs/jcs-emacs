;;; lang/pascal/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-pascal-template "pascal" "default.txt"
  "Header for Pascal header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'pascal-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]pas")
                              'jcs-insert-pascal-template))
