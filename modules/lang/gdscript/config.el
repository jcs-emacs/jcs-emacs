;;; lang/gdscript/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-gdscript-template "gdscript" "default.txt"
  "Header for Godot Script header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'gdscript-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]gd")
                              'jcs-insert-gdscript-template))
