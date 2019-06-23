;;; jcs-shader-mode.el --- Shader mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'shader-mode)


(defun jcs-shader-mode-hook ()
  "Shader mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]shader" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-shader-template))
          ))

  )
(add-hook 'shader-mode-hook 'jcs-shader-mode-hook)


(defun jcs-glsl-script-format ()
  "Format the given file as a GLSL shader file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-glsl-template)))

(require 'glsl-mode)
(defun jcs-glsl-mode-hook ()
  "GLSL mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]frag" buffer-file-name) (jcs-glsl-script-format))
          ((string-match "[.]geom" buffer-file-name) (jcs-glsl-script-format))
          ((string-match "[.]glsl" buffer-file-name) (jcs-glsl-script-format))
          ((string-match "[.]vert" buffer-file-name) (jcs-glsl-script-format))
          ))

  ;; Normal
  (define-key glsl-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key glsl-mode-map (kbd "C-c C-c") #'kill-ring-save)
  )
(add-hook 'glsl-mode-hook 'jcs-glsl-mode-hook)


(provide 'jcs-shader-mode)
;;; jcs-shader-mode.el ends here
