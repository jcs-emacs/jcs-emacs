;;; lang/shader/config.el  -*- lexical-binding: t; -*-

(require 'cc-mode)

(require 'shader-mode)
(require 'glsl-mode)
(require 'hlsl-mode)
(require 'wgsl-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-shader-unlit-template "shader" "Unlit.txt"
  "Header for Shader Unlit file.")

(file-header-defins jcs-insert-shader-image-effect-template "shader" "ImageEffect.txt"
  "Header for Shader ImageEffect file.")

(file-header-defins jcs-insert-shader-surface-template "shader" "Surface.txt"
  "Header for Shader Surface file.")

(file-header-defsrc jcs-ask-shader-template "Select Shader template: "
  '(("Unlit"       . "Effects or unique objects in your visuals that don’t need lighting")
    ("ImageEffect" . "Are a way of post-processing rendered image")
    ("Surface"     . "Shaders that interact with lighting is complex"))
  (pcase index
    (0 (jcs-insert-shader-unlit-template))
    (1 (jcs-insert-shader-image-effect-template))
    (2 (jcs-insert-shader-surface-template))))

(file-header-defins jcs-insert-glsl-template "glsl" "default.txt"
  "Header for GLSL file.")

(file-header-defins jcs-insert-hlsl-template "hlsl" "default.txt"
  "Header for HLSL file.")

(file-header-defins jcs-insert-wgsl-template "wgsl" "default.txt"
  "Header for WGSL file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'shader-mode-hook
  (modify-syntax-entry ?# "w")

  (jcs-use-cc-mutliline-comment)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]shader")
                              'jcs-ask-shader-template
                              :interactive t))

(jcs-add-hook 'glsl-mode-hook
  (modify-syntax-entry ?# "w")

  (company-fuzzy-backend-add-before 'company-glsl 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]frag" "[.]geom" "[.]glsl" "[.]vert")
                              'jcs-insert-glsl-template))

(jcs-add-hook 'hlsl-mode-hook
  (modify-syntax-entry ?# "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]fx" "[.]hlsl")
                              'jcs-insert-hlsl-template))

(jcs-add-hook 'wgsl-mode-hook
  (modify-syntax-entry ?# "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]wgsl")
                              'jcs-insert-hlsl-template))
