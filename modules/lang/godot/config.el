;;; lang/godot/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-gdscript-template "godot/gdscript" "default.txt"
  "Header for Godot Script header file.")

(file-header-defins jcs-insert-gdshader-template "godot/gdshader" "default.txt"
  "Header for Godot Shader header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'gdscript-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]gd")
                              'jcs-insert-gdscript-template))

(jcs-add-hook 'gdshader-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]gdshader")
                              'jcs-insert-gdshader-template))
