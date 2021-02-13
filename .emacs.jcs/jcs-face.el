;;; jcs-face.el --- Face related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Regular Faces" )
;;

(defun jcs--set-common-face (face color-fg)
  "Set the FACE foreground COLOR-FG."
  (set-face-attribute face nil :foreground color-fg))

(defun jcs--apply-face-map (face-map)
  "Apply FACE-MAP with function `jcs--set-common-face'."
  (let ((light-theme-p (jcs-is-light-theme-p)))
    (dolist (face-data face-map)
      (let ((face (car face-data)) (color (cdr face-data)))
        (setq color
              (cond ((listp color)
                     (if (= (length color) 1) (nth 0 color)
                       (if light-theme-p (nth 0 color) (nth 1 color))))
                    (t color)))
        (jcs--set-common-face face color)))))

(defun jcs-reset-common-faces-by-theme ()
  "Reset comment faces case on the theme."
  (let ((face-map
         `((font-lock-builtin-face . ("light steel blue"))
           (font-lock-comment-face . ("olive drab"))
           (font-lock-constant-face . ("#2B91AF" "#38EFCA"))
           (font-lock-doc-face . ("olive drab"))
           (font-lock-function-name-face . ("#74534B" "#D2D2D2"))
           (font-lock-keyword-face . ("#0000FF" "#17A0FB"))
           (font-lock-preprocessor-face . ("#808080" "#8D9B99"))
           (font-lock-string-face . ("#B21515" "#D69D78"))
           (font-lock-type-face . ("#2B91AF" "#38EFCA"))
           (font-lock-variable-name-face . ("#000000" "#D2D2D2")))))
    (jcs--apply-face-map face-map))

  (when (featurep 'tree-sitter-hl)
    (let ((face-map
           `((tree-sitter-hl-face:tag . ("#900022" "#D7A552"))
             (tree-sitter-hl-face:type.builtin . ("#0000FF" "#17A0FB"))
             (tree-sitter-hl-face:type . ("#2B91AF" "#38EFCA"))
             (tree-sitter-hl-face:function . ("black" "#D2D2D2"))
             (tree-sitter-hl-face:function.call . ("black" "#D2D2D2"))
             (tree-sitter-hl-face:variable.parameter . ("#808080" "#7F7F7F"))
             (tree-sitter-hl-face:property . ("#2F4F4F" "#B5CEA8"))
             (tree-sitter-hl-face:property.definition . ("#2F4F4F" "#B5CEA8"))
             (tree-sitter-hl-face:punctuation . ("#020000" "#B4B4B3"))
             (tree-sitter-hl-face:operator . ("#020000" "#B4B4B3"))
             (tree-sitter-hl-face:number . ("black" "#B5CEA8"))
             (tree-sitter-hl-face:constant . ("#6F008A" "#B363BE"))
             (tree-sitter-hl-face:constant.builtin . ("#0000FF" "#17A0FB"))
             (tree-sitter-hl-face:keyword . ("#0000FF" "#17A0FB"))
             (tree-sitter-hl-face:variable . ("#000000" "#D2D2D2"))
             (tree-sitter-hl-face:variable.special . ("#6F008A" "#B363BE")))))
      (jcs--apply-face-map face-map))))

;;
;; (@* "Common" )
;;

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

;;
;; (@* "Load face order" )
;;

(with-eval-after-load 'cc-mode
  (require 'jcs-preproc) (jcs-init-preproc-faces)
  (jcs-init-java-faces))
(with-eval-after-load 'markdown-mode (jcs-init-markdown-faces))
(with-eval-after-load 'org (jcs-init-org-faces))
(with-eval-after-load 'typescript-mode (jcs-init-typescript-faces))
(with-eval-after-load 'web-mode (jcs-init-web-faces))

(provide 'jcs-face)
;;; jcs-face.el ends here
