;;; lang/renpy/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-renpy-template "renpy" "default.txt"
  "Header for Ren'Py.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'renpy-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("\\.rpym?\\'")
                              'jcs-insert-renpy-template))
