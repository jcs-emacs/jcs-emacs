;;; lang/jayces/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-jayces-template "jayces" "default.txt"
  "Header for JayCeS header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'jayces-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]jcs"
                                "[.]jayces")
                              'jcs-insert-jayces-template))
