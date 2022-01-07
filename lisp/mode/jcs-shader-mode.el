;;; jcs-shader-mode.el --- Shader mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'shader-mode)
(require 'glsl-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-shader-template ()
  "Header for Shader header file."
  (jcs--file-header--insert "shader" "default_shader.txt"))

(defun jcs-insert-glsl-template ()
  "Header for GLSL header file."
  (jcs--file-header--insert "shader" "default_glsl.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'shader-mode-hook
  (jcs-use-cc-mutliline-comment)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]shader")
                              'jcs-insert-shader-template))

(jcs-add-hook 'glsl-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]frag"
                                "[.]geom"
                                "[.]glsl"
                                "[.]vert")
                              'jcs-insert-glsl-template))

(provide 'jcs-shader-mode)
;;; jcs-shader-mode.el ends here