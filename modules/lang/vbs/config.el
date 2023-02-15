;;; lang/vbs/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-vbs-template "vbs" "default.txt"
  "Header format for BASIC file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'visual-basic-mode-hook
  (run-hooks 'prog-mode-hook)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]vbs")
                              'jcs-insert-vbs-template))
