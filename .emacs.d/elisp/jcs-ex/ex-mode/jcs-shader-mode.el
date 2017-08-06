;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-shader-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Mon Oct 17 13:51:49 EST 2017>
;; Time-stamp: <2017-07-17 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-function is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-function is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Shader mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'shader-mode)

(defvar jcs-shader-mode-map nil "Keymap for `jcs-shader-mode'")
(progn
  (setq jcs-shader-mode-map (make-sparse-keymap))

  ;; comment block
  (define-key jcs-shader-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key jcs-shader-mode-map (kbd "*") 'jcs-c-comment-pair)
  )

;;;
;; TOPIC(jayces): Elisp: How to Create Keymap for Major Mode
;; URL(jayces): http://ergoemacs.org/emacs/elisp_create_major_mode_keymap.html
(define-derived-mode jcs-shader-mode ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  (shader-mode)

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (defun jcs-shader-format ()
    "Format the given file as a shader script. - JenChieh Unity CG Shader."
    (interactive)

    (jcs-global-file-info)

    (insert "\n\n")

    (insert "Shader \"\"\n")
    (insert "{\n")

    (insert "Properties\n")
    (insert "{\n")
    (insert "_MainTex (\"Texture\", 2D) = \"white\" {}\n")
    (insert "}\n")

    (insert "SubShader\n")
    (insert "{\n")

    (insert "Tags { \"\"=\"\" }\n")

    (insert "Pass\n")
    (insert "{\n")
    (insert "CGPROGRAM\n\n")
    (insert "#pragma vertex vert\n")
    (insert "#pragma fragment frag\n")
    (insert "#include \"UnityCG.cginc\"\n\n")

    (insert "struct appdata\n")
    (insert "{\n");
    (insert "float4 vertex : POSITION;\n")
    (insert "float2 uv : TEXCOORD0;\n");
    (insert "};\n\n")

    (insert "struct v2f\n")
    (insert "{\n")
    (insert "float4 vertex : SV_POSITION;\n")
    (insert "float2 uv : TEXCOORD0;\n")
    (insert "};\n\n")

    (insert "float4 _MainTex_ST;\n\n")

    (insert "v2f vert(appdata v)\n")
    (insert "{\n")
    (insert "v2f o;\n")
    (insert "o.vertex = mul(UNITY_MATRIX_MVP, v.vertex);\n")
    (insert "o.uv = TRANSFORM_TEX(v.uv, _MainTex);\n")
    (insert "return o;\n")
    (insert "}\n\n")

    (insert "sampler2D _MainTex;\n\n")

    (insert "fixed4 frag (v2f i) : SV_Target\n")
    (insert "{\n")
    (insert "half4 c = tex2D (_MainTex, i.uv);\n")
    (insert "return c;\n")
    (insert "}\n\n")

    (insert "ENDCG\n")
    (insert "}\n")

    (insert "}\n")
    (insert "}\n")

    ;; format the document once.
    (jcs-format-document)

    ;; Move to beginning of the buffer.
    (beginning-of-buffer)
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]shader" buffer-file-name) (jcs-shader-format))
        )

  ;; actually no need
  (use-local-map jcs-shader-mode-map)
  )
(add-to-list 'auto-mode-alist '("\\.shader?\\'" . jcs-shader-mode))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-shader-mode.el file
