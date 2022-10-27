;;; lang/basic/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-basic-template "basic" "default.txt"
  "Header format for BASIC file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'basic-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]bas")
                              'jcs-insert-basic-template))
