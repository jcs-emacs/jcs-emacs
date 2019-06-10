;;; jcs-face.el --- Face related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------
;; Regular Faces
;;----------------------------------------------

(defface jcs-font-lock-builtin-face
  '((t (:foreground "light steel blue")))
  "JCS local builtin face."
  :group 'jcs)
(defvar jcs-font-lock-builtin-face 'jcs-font-lock-builtin-face)

(defface jcs-font-lock-comment-face
  '((t (:foreground "olive drab")))
  "JCS local comment face."
  :group 'jcs)
(defvar jcs-font-lock-comment-face 'jcs-font-lock-comment-face)

(defface jcs-font-lock-constant-face
  '((t (:foreground "#38EFCA")))
  "JCS local constant face."
  :group 'jcs)
(defvar jcs-font-lock-constant-face 'jcs-font-lock-constant-face)

(defface jcs-font-lock-doc-face
  '((t (:foreground "olive drab")))
  "JCS local doc face."
  :group 'jcs)
(defvar jcs-font-lock-doc-face 'jcs-font-lock-doc-face)

(defface jcs-font-lock-function-name-face
  '((t (:foreground "#D2D2D2")))
  "JCS local function name face."
  :group 'jcs)
(defvar jcs-font-lock-function-name-face 'jcs-font-lock-function-name-face)

(defface jcs-font-lock-keyword-face
  '((t (:foreground "#17A0FB")))
  "JCS local keyword face."
  :group 'jcs)
(defvar jcs-font-lock-keyword-face 'jcs-font-lock-keyword-face)

(defface jcs-font-lock-preprocessor-face
  '((t (:foreground "#8D9B99")))
  "JCS local preprocessor face."
  :group 'jcs)
(defvar jcs-font-lock-preprocessor-face 'jcs-font-lock-preprocessor-face)

(defface jcs-font-lock-string-face
  '((t (:foreground "#D69D78")))
  "JCS local string face."
  :group 'jcs)
(defvar jcs-font-lock-string-face 'jcs-font-lock-string-face)

(defface jcs-font-lock-type-face
  '((t (:foreground "#38EFCA")))
  "JCS local type face."
  :group 'jcs)
(defvar jcs-font-lock-type-face 'jcs-font-lock-type-face)

(defface jcs-font-lock-variable-name-face
  '((t (:foreground "#D2D2D2")))
  "JCS local variable name face."
  :group 'jcs)
(defvar jcs-font-lock-variable-name-face 'jcs-font-lock-variable-name-face)


;;;###autoload
(defun jcs-init-set-face ()
  "Set JayCeS's hightlight faces.
For those mode does not apply faces correctly!"
  (interactive)
  (face-remap-add-relative 'font-lock-builtin-face '(jcs-font-lock-builtin-face))
  (face-remap-add-relative 'font-lock-comment-face '(jcs-font-lock-comment-face))
  (face-remap-add-relative 'font-lock-constant-face '(jcs-font-lock-constant-face))
  (face-remap-add-relative 'font-lock-doc-face '(jcs-font-lock-doc-face))
  (face-remap-add-relative 'font-lock-function-name-face '(jcs-font-lock-function-name-face))
  (face-remap-add-relative 'font-lock-keyword-face '(jcs-font-lock-keyword-face))
  (face-remap-add-relative 'font-lock-preprocessor-face '(jcs-font-lock-preprocessor-face))
  (face-remap-add-relative 'font-lock-string-face '(jcs-font-lock-string-face))
  (face-remap-add-relative 'font-lock-type-face '(jcs-font-lock-type-face))
  (face-remap-add-relative 'font-lock-variable-name-face '(jcs-font-lock-variable-name-face)))

(set-face-attribute 'font-lock-builtin-face nil :foreground (face-foreground jcs-font-lock-builtin-face))
(set-face-attribute 'font-lock-comment-face nil :foreground (face-foreground jcs-font-lock-comment-face))
(set-face-attribute 'font-lock-constant-face nil :foreground (face-foreground jcs-font-lock-constant-face))
(set-face-attribute 'font-lock-doc-face nil :foreground (face-foreground jcs-font-lock-doc-face))
(set-face-attribute 'font-lock-function-name-face nil :foreground (face-foreground jcs-font-lock-function-name-face))
(set-face-attribute 'font-lock-keyword-face nil :foreground (face-foreground jcs-font-lock-keyword-face))
(set-face-attribute 'font-lock-preprocessor-face nil :foreground (face-foreground jcs-font-lock-preprocessor-face))
(set-face-attribute 'font-lock-string-face nil :foreground (face-foreground jcs-font-lock-string-face))
(set-face-attribute 'font-lock-type-face nil :foreground (face-foreground jcs-font-lock-type-face))
(set-face-attribute 'font-lock-variable-name-face nil :foreground (face-foreground jcs-font-lock-variable-name-face))


;;----------------------------------------------
;; Object Oriented Programming
;;----------------------------------------------

(defface jcs-oop-tag-face
  '((t (:foreground "#38EFCA")))
  "Highlight OOP tag."
  :group 'jcs)
(defvar jcs-oop-tag-face 'jcs-oop-tag-face)

(defface jcs-oop-type-face
  '((t (:foreground "SteelBlue")))
  "Highlight OOP type."
  :group 'jcs)
(defvar jcs-oop-type-face 'jcs-oop-type-face)

(defface jcs-oop-value-face
  '((t (:foreground "LightCoral")))
  "Highlight OOP value."
  :group 'jcs)
(defvar jcs-oop-value-face 'jcs-oop-value-face)


;;----------------------------------------------
;; Preprocessor
;;----------------------------------------------

(defface jcs-preproc-variable-name-face
  '((t (:foreground "#B363BE")))
  "Highlight OOP tag."
  :group 'jcs)
(defvar jcs-preproc-variable-name-face 'jcs-preproc-variable-name-face)


;;----------------------------------------------
;; Java
;;----------------------------------------------

(defface jcs-font-lock-null-face
  '((t (:foreground "LightSteelBlue")))
  "Java null face."
  :group 'jcs)
(defvar jcs-font-lock-null-face 'jcs-font-lock-null-face)

(defface jcs-font-lock-void-face
  '((t (:foreground "LightSteelBlue")))
  "Java void face."
  :group 'jcs)
(defvar jcs-font-lock-void-face 'jcs-font-lock-void-face)


;;----------------------------------------------
;; Python
;;----------------------------------------------

(defface jcs-py-mode-docstring-face
  '((t (:foreground "olive drab")))
  "Python mode docstring face."
  :group 'jcs)
(defvar jcs-py-mode-docstring-face 'jcs-py-mode-docstring-face)


;;----------------------------------------------
;; Web
;;----------------------------------------------

(defface jcs-web-mode-block-comment-face
  '((t (:inherit 'jcs-font-lock-comment-face :background "#000000")))
  "Web mode block comment face with dark background."
  :group 'jcs)
(defvar jcs-web-mode-block-comment-face 'jcs-web-mode-block-comment-face)

(defface jcs-web-mode-html-attr-value-face
  '((t (:foreground "olive drab")))
  "Highlight HTML value."
  :group 'jcs)
(defvar jcs-web-mode-html-attr-value-face 'jcs-web-mode-html-attr-value-face)


(defface jcs-css-type-face
  '((t (:foreground "#38EFCA")))
  "Highlight CSS value."
  :group 'jcs)
(defvar jcs-css-type-face 'jcs-css-type-face)

(defface jcs-css-value-face
  '((t (:foreground "#D2D2D2")))
  "Highlight CSS value."
  :group 'jcs)
(defvar jcs-css-value-face 'jcs-css-value-face)

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


;;----------------------------------------------
;; Load face order.
;;----------------------------------------------

(with-eval-after-load 'preproc-font-lock (jcs-init-preproc-faces))

(with-eval-after-load 'cc-mode (jcs-init-java-faces))
(with-eval-after-load 'css-mode (jcs-init-css-faces))
(with-eval-after-load 'lua-mode (jcs-init-lua-faces))
(with-eval-after-load 'python-mode (jcs-init-py-faces))
(with-eval-after-load 'web-mode (jcs-init-web-faces))

;; Load OOP faces.
(when (fboundp 'jcs-oop-reload-faces)
  (jcs-oop-reload-faces))


(provide 'jcs-face)
;;; jcs-face.el ends here
