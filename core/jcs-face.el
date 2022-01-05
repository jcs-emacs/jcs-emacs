;;; jcs-face.el --- Face related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Common" )
;;

(defun jcs-face-fg (face fg)
  "Set the FACE foreground FG."
  (set-face-attribute face nil :foreground fg))

(defface jcs-font-lock-null-face
  '((t (:foreground "LightSteelBlue")))
  "Face for keywords like `null', `void', `nil', `undefined'."
  :group 'jcs)
(defvar jcs-font-lock-null-face 'jcs-font-lock-null-face)

;;
;; (@* "Preprocessor" )
;;

(defface jcs-preproc-comment-face
  '((default :inherit font-lock-preprocessor-face))
  "Face for preprocessor comment."
  :group 'jcs)
(defvar jcs-preproc-comment-face 'jcs-preproc-comment-face)

(defface jcs-preproc-comment-type-face
  '((default :inherit font-lock-preprocessor-face))
  "Face for preprocessor comment type."
  :group 'jcs)
(defvar jcs-preproc-comment-type-face 'jcs-preproc-comment-type-face)

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

(provide 'jcs-face)
;;; jcs-face.el ends here
