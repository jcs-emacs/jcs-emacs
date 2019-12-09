;;; jcs-face.el --- Face related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------
;; Object Oriented Programming

(defface jcs-oop-tag-face
  '((t (:foreground "SlateGray")))
  "Highlight OOP tag."
  :group 'jcs)
(defvar jcs-oop-tag-face 'jcs-oop-tag-face)

(defface jcs-oop-type-face
  '((t (:foreground "SteelBlue")))
  "Highlight OOP type."
  :group 'jcs)
(defvar jcs-oop-type-face 'jcs-oop-type-face)

(defface jcs-oop-value-face
  '((t (:foreground "gold4")))
  "Highlight OOP value."
  :group 'jcs)
(defvar jcs-oop-value-face 'jcs-oop-value-face)


;;----------------------------------------------
;; Preprocessor

(defface jcs-preproc-variable-name-face
  '((t (:foreground "#B363BE")))
  "Highlight preprocessor operator."
  :group 'jcs)
(defvar jcs-preproc-variable-name-face 'jcs-preproc-variable-name-face)


;;----------------------------------------------
;; Java

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
;; Web

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


;;------------------------------------------------------------------------------------------------------
;; Load face order.
;;------------------------------------------------------------------------------------------------------

(with-eval-after-load 'preproc-font-lock (jcs-init-preproc-faces))

(with-eval-after-load 'cc-mode (jcs-init-java-faces))
(with-eval-after-load 'css-mode (jcs-init-css-faces))
(with-eval-after-load 'lua-mode (jcs-init-lua-faces))
(with-eval-after-load 'org (jcs-init-org-faces))
(with-eval-after-load 'python-mode (jcs-init-py-faces))
(with-eval-after-load 'web-mode (jcs-init-web-faces))

;; Load OOP faces.
(add-hook 'prog-mode-hook
          (lambda ()
            (jcs-oop-reload-faces)
            (jcs-oop-complete-missing-font)))


(provide 'jcs-face)
;;; jcs-face.el ends here
