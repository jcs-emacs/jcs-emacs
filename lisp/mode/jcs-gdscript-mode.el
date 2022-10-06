;;; jcs-gdscript-mode.el --- Godot Script mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'gdscript-mode)

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

(provide 'jcs-gdscript-mode)
;;; jcs-gdscript-mode.el ends here
