;;; lang/shader/config.el  -*- lexical-binding: t; -*-

(require 'shader-mode)
(require 'glsl-mode)
(require 'hlsl-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-shader-template "shader" "default.txt"
  "Header for Shader header file.")

(file-header-defins jcs-insert-glsl-template "glsl" "default.txt"
  "Header for GLSL header file.")

(file-header-defins jcs-insert-hlsl-template "hlsl" "default.txt"
  "Header for HLSL header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'shader-mode-hook
  (modify-syntax-entry ?_ "w")

  (jcs-use-cc-mutliline-comment)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]shader")
                              'jcs-insert-shader-template))

(jcs-add-hook 'glsl-mode-hook
  (modify-syntax-entry ?_ "w")

  (company-fuzzy-backend-add 'company-glsl)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]frag" "[.]geom" "[.]glsl" "[.]vert")
                              'jcs-insert-glsl-template))

(jcs-add-hook 'hlsl-mode-hook
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]fx" "[.]hlsl")
                              'jcs-insert-hlsl-template))
