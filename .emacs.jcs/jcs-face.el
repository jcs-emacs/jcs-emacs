;;; jcs-face.el --- Face related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Regular Faces" )
;;

(defun jcs--set-common-face (face color-fg)
  "Set the FACE foreground COLOR-FG."
  (set-face-attribute face nil :foreground color-fg))

(defun jcs-reset-common-faces-by-theme ()
  "Reset comment faces case on the theme."
  (let ((light-theme-p (jcs-is-light-theme-p)))
    (if light-theme-p
        (progn
          (jcs--set-common-face 'font-lock-builtin-face "light steel blue")
          (jcs--set-common-face 'font-lock-comment-face "olive drab")
          (jcs--set-common-face 'font-lock-constant-face "#2B91AF")
          (jcs--set-common-face 'font-lock-doc-face "olive drab")
          (jcs--set-common-face 'font-lock-function-name-face "#74534B")
          (jcs--set-common-face 'font-lock-keyword-face "#0000FF")
          (jcs--set-common-face 'font-lock-preprocessor-face "#808080")
          (jcs--set-common-face 'font-lock-string-face "#B21515")
          (jcs--set-common-face 'font-lock-type-face "#2B91AF")
          (jcs--set-common-face 'font-lock-variable-name-face "#000000"))
      (jcs--set-common-face 'font-lock-builtin-face "light steel blue")
      (jcs--set-common-face 'font-lock-comment-face "olive drab")
      (jcs--set-common-face 'font-lock-constant-face "#38EFCA")
      (jcs--set-common-face 'font-lock-doc-face "olive drab")
      (jcs--set-common-face 'font-lock-function-name-face "#D2D2D2")
      (jcs--set-common-face 'font-lock-keyword-face "#17A0FB")
      (jcs--set-common-face 'font-lock-preprocessor-face "#8D9B99")
      (jcs--set-common-face 'font-lock-string-face "#D69D78")
      (jcs--set-common-face 'font-lock-type-face "#38EFCA")
      (jcs--set-common-face 'font-lock-variable-name-face "#D2D2D2"))

    (when (featurep 'tree-sitter-hl)
      (if light-theme-p
          (progn
            (jcs--set-common-face 'tree-sitter-hl-face:type.builtin "#2B91AF")
            (jcs--set-common-face 'tree-sitter-hl-face:type "#2B91AF")
            (jcs--set-common-face 'tree-sitter-hl-face:function "black")
            (jcs--set-common-face 'tree-sitter-hl-face:function.call "black")
            (jcs--set-common-face 'tree-sitter-hl-face:variable.parameter "#808080")
            (jcs--set-common-face 'tree-sitter-hl-face:property.definition "black")
            (jcs--set-common-face 'tree-sitter-hl-face:property "black")
            (jcs--set-common-face 'tree-sitter-hl-face:operator "#020000")
            (jcs--set-common-face 'tree-sitter-hl-face:number "black")
            (jcs--set-common-face 'tree-sitter-hl-face:constant "#6F008A")
            (jcs--set-common-face 'tree-sitter-hl-face:variable.special "#6F008A")
            )
        (jcs--set-common-face 'tree-sitter-hl-face:type.builtin "#38EFCA")
        (jcs--set-common-face 'tree-sitter-hl-face:type "#38EFCA")
        (jcs--set-common-face 'tree-sitter-hl-face:function "#D2D2D2")
        (jcs--set-common-face 'tree-sitter-hl-face:function.call "#D2D2D2")
        (jcs--set-common-face 'tree-sitter-hl-face:variable.parameter "#7F7F7F")
        (jcs--set-common-face 'tree-sitter-hl-face:property.definition "#D2D2D2")
        (jcs--set-common-face 'tree-sitter-hl-face:property "#D2D2D2")
        (jcs--set-common-face 'tree-sitter-hl-face:operator "#B4B4B3")
        (jcs--set-common-face 'tree-sitter-hl-face:number "#B5CEA8")
        (jcs--set-common-face 'tree-sitter-hl-face:constant "#B363BE")
        (jcs--set-common-face 'tree-sitter-hl-face:variable.special "#B363BE")
        ))))

;;
;; (@* "Common" )
;;

(defface jcs-font-lock-null-face
  '((t (:foreground "LightSteelBlue")))
  "Face for keywords like `null', `void', `nil', `undefined'."
  :group 'jcs)
(defvar jcs-font-lock-null-face 'jcs-font-lock-null-face)

;;
;; (@* "Web" )
;;

(defface jcs-web-mode-block-face
  '((t (:inherit 'default :background "#000000")))
  "Web mode block face with dark background."
  :group 'jcs)
(defvar jcs-web-mode-block-face 'jcs-web-mode-block-face)

(defface jcs-web-mode-block-comment-face
  '((t (:inherit 'font-lock-comment-face :background "#000000")))
  "Web mode block comment face with dark background."
  :group 'jcs)
(defvar jcs-web-mode-block-comment-face 'jcs-web-mode-block-comment-face)

(defface jcs-web-mode-html-attr-value-face
  '((t (:foreground "olive drab")))
  "Highlight HTML value."
  :group 'jcs)
(defvar jcs-web-mode-html-attr-value-face 'jcs-web-mode-html-attr-value-face)

(defface jcs-css-selector-face
  '((t (:foreground "#17A0FB")))
  "Highlight CSS selector."
  :group 'jcs)
(defvar jcs-css-selector-face 'jcs-css-selector-face)

(defface jcs-css-type-face
  '((t (:foreground "#38EFCA")))
  "Highlight CSS value."
  :group 'jcs)
(defvar jcs-css-type-face 'jcs-css-type-face)

(defface jcs-css-id-face
  '((t (:foreground "#D68974")))
  "Highlight CSS id."
  :group 'jcs)
(defvar jcs-css-id-face 'jcs-css-id-face)

(defface jcs-css-class-face
  '((t (:foreground "#FAD42D")))
  "Highlight CSS class."
  :group 'jcs)
(defvar jcs-css-class-face 'jcs-css-class-face)

(defface jcs-css-event-face
  '((t (:foreground "#B592EA")))
  "Highlight CSS event."
  :group 'jcs)
(defvar jcs-css-event-face 'jcs-css-event-face)

(defface jcs-css-number-face
  '((t (:foreground "#B5CE89")))
  "Highlight CSS number."
  :group 'jcs)
(defvar jcs-css-number-face 'jcs-css-number-face)

(defface jcs-css-variable-face
  '((t (:foreground "#F092FE")))
  "Highlight CSS variable."
  :group 'jcs)
(defvar jcs-css-variable-face 'jcs-css-variable-face)

;;
;; (@* "Load face order" )
;;

(with-eval-after-load 'cc-mode (jcs-init-java-faces))
(with-eval-after-load 'css-mode (jcs-init-css-faces))
(with-eval-after-load 'markdown-mode (jcs-init-markdown-faces))
(with-eval-after-load 'org (jcs-init-org-faces))
(with-eval-after-load 'typescript-mode (jcs-init-typescript-faces))
(with-eval-after-load 'web-mode (jcs-init-web-faces))

(provide 'jcs-face)
;;; jcs-face.el ends here
