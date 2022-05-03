;;; jcs-shader-mode.el --- Shader mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'shader-mode)
(require 'glsl-mode)
(require 'hlsl-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-shader-template ()
  "Header for Shader header file."
  (jcs--file-header--insert "shader" "default.txt"))

(defun jcs-insert-glsl-template ()
  "Header for GLSL header file."
  (jcs--file-header--insert "glsl" "default.txt"))

(defun jcs-insert-hlsl-template ()
  "Header for HLSL header file."
  (jcs--file-header--insert "hlsl" "default.txt"))

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
  (company-fuzzy-backend-add 'company-glsl)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]frag" "[.]geom" "[.]glsl" "[.]vert")
                              'jcs-insert-glsl-template))

(jcs-add-hook 'hlsl-mode-hook
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]fx" "[.]hlsl")
                              'jcs-insert-hlsl-template))

(provide 'jcs-shader-mode)
;;; jcs-shader-mode.el ends here
