;;; jcs-gdscript-mode.el --- Godot Script mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'gdscript-mode)

(defun jcs-gdscript-mode-hook ()
  "Godot Script mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]gd")
                              'jcs-insert-gdscript-template))

(add-hook 'gdscript-mode-hook 'jcs-gdscript-mode-hook)

(provide 'jcs-gdscript-mode)
;;; jcs-gdscript-mode.el ends here
