;;; lang/p4/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-p4-template "p4" "default.txt"
  "Header for P4 header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'p4lang-mode-hook
  (run-hooks 'prog-mode-hook)

  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]p4")
                              'jcs-insert-p4-template))
