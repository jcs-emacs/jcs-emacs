;;; jcs-shader-mode.el --- Shader mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'shader-mode)
(require 'glsl-mode)


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

  ;; File Header
  (jcs-insert-header-if-valid '("[.]shader")
                              'jcs-insert-shader-template)

  )
(add-hook 'shader-mode-hook 'jcs-shader-mode-hook)


(defun jcs-glsl-mode-hook ()
  "GLSL mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]frag"
                                "[.]geom"
                                "[.]glsl"
                                "[.]vert")
                              'jcs-insert-glsl-template)
  )
(add-hook 'glsl-mode-hook 'jcs-glsl-mode-hook)


(provide 'jcs-shader-mode)
;;; jcs-shader-mode.el ends here
