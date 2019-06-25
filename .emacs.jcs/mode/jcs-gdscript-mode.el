;;; jcs-gdscript-mode.el --- Godot Script mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'gdscript-mode)


(defun jcs-gdscript-mode-hook ()
  "Godot Script mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]gd" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-gdscript-template))
          ))

  ;; Normal
  (define-key gdscript-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key gdscript-mode-map (kbd "C-c C-c") #'kill-ring-save)
  )
(add-hook 'gdscript-mode-hook 'jcs-gdscript-mode-hook)


(provide 'jcs-gdscript-mode)
;;; jcs-gdscript-mode.el ends here
