;;; jcs-face.el --- Face related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------
;; Regular Faces
;;----------------------------------------------

(defface jcs-font-lock-comment-face
  '((t (:foreground "olive drab")))
  "JCS local comment face."
  :group 'basic-faces)
(defvar jcs-font-lock-comment-face 'jcs-font-lock-comment-face)

(defface jcs-font-lock-string-face
  '((t (:foreground "#D69D78")))
  "JCS local string face."
  :group 'basic-faces)
(defvar jcs-font-lock-string-face 'jcs-font-lock-string-face)

(defface jcs-font-lock-preprocessor-face
  '((t (:foreground "#8D9B99")))
  "JCS local preprocessor face."
  :group 'basic-faces)
(defvar jcs-font-lock-preprocessor-face 'jcs-font-lock-preprocessor-face)

(defun jcs-init-set-face ()
  "Set JayCeS's hightlight faces.
For those mode does not apply faces correctly!"
  (face-remap-add-relative 'font-lock-comment-face '(jcs-font-lock-comment-face))
  (face-remap-add-relative 'font-lock-doc-face '(jcs-font-lock-comment-face))
  (face-remap-add-relative 'font-lock-string-face '(jcs-font-lock-string-face))
  (face-remap-add-relative 'font-lock-preprocessor-face '(jcs-font-lock-preprocessor-face)))

;; Apply JayCeS's hightlight faces.
(setq font-lock-comment-face jcs-font-lock-comment-face)
(setq font-lock-doc-face jcs-font-lock-comment-face)
(setq font-lock-string-face jcs-font-lock-string-face)
(setq font-lock-preprocessor-face jcs-font-lock-preprocessor-face)


;;----------------------------------------------
;; Fixme Faces
;;----------------------------------------------

;; Bright-red TODOs
(defvar jcs-fixme-modes '(actionscript-mode
                          bat-mode
                          basic-mode
                          cc-mode
                          c-mode
                          c++-mode
                          clojure-mode
                          cobol-mode
                          cmake-mode
                          csharp-mode
                          css-mode
                          emacs-lisp-mode
                          go-mode
                          haskell-mode
                          haxe-mode
                          java-mode
                          js2-mode
                          lisp-mode
                          lua-mode
                          markdown-mode
                          nasm-mode
                          objc-mode
                          org-mode
                          perl-mode
                          php-mode
                          python-mode
                          ruby-mode
                          rust-mode
                          scala-mode
                          sh-mode
                          sql-mode
                          swift-mode
                          typescript-mode
                          verilog-mode
                          vimrc-mode
                          web-mode
                          yaml-mode
                          )
  "Modes that apply fixme request.")

;; List of color: https://alexschroeder.ch/geocities/kensanata/colors.html
(defface jcs-font-lock-fixme-face
  '((t (:foreground "red" :underline t :weight bold)))
  "Highlight word 'TODO'.")
(defvar jcs-font-lock-fixme-face 'jcs-font-lock-fixme-face)

(defface jcs-font-lock-attention-face
  '((t (:foreground "red" :underline t :weight bold)))
  "Highlight word 'ATTENTION'.")
(defvar jcs-font-lock-attention-face 'jcs-font-lock-attention-face)

(defface jcs-font-lock-study-face
  '((t (:foreground "yellow" :underline t :weight bold)))
  "Highlight word 'STUDY'.")
(defvar jcs-font-lock-study-face 'jcs-font-lock-study-face)

(defface jcs-font-lock-important-face
  '((t (:foreground "yellow" :underline t :weight bold)))
  "Highlight word 'IMPORTANT'.")
(defvar jcs-font-lock-important-face 'jcs-font-lock-important-face)

(defface jcs-font-lock-caution-face
  '((t (:foreground "yellow" :underline t :weight bold)))
  "Highlight word 'CAUTION'.")
(defvar jcs-font-lock-caution-face 'jcs-font-lock-caution-face)

(defface jcs-font-lock-optimize-face
  '((t (:foreground "yellow" :underline t :weight bold)))
  "Highlight word 'OPTIMIZE'.")
(defvar jcs-font-lock-optimize-face 'jcs-font-lock-optimize-face)

(defface jcs-font-lock-note-face
  '((t (:foreground "dark green" :underline t :weight bold)))
  "Highlight word 'NOTE'.")
(defvar jcs-font-lock-note-face 'jcs-font-lock-note-face)

(defface jcs-font-lock-description-face
  '((t (:foreground "dark green" :underline t :weight bold)))
  "Highlight word 'DESCRIPTION'.")
(defvar jcs-font-lock-description-face 'jcs-font-lock-description-face)

(defface jcs-font-lock-tag-face
  '((t (:foreground "dark green" :underline t :weight bold)))
  "Highlight word 'TAG'.")
(defvar jcs-font-lock-tag-face 'jcs-font-lock-tag-face)

(defface jcs-font-lock-debugging-face
  '((t (:foreground "turquoise" :underline t :weight bold)))
  "Highlight word 'DEBUGGING'.")
(defvar jcs-font-lock-debugging-face 'jcs-font-lock-debugging-face)

(defface jcs-font-lock-temporary-face
  '((t (:foreground "turquoise" :underline t :weight bold)))
  "Highlight word 'TEMPORARY'.")
(defvar jcs-font-lock-temporary-face 'jcs-font-lock-temporary-face)

(defface jcs-font-lock-source-face
  '((t (:foreground "PaleTurquoise2" :underline t :weight bold)))
  "Highlight word 'SOURCE'.")
(defvar jcs-font-lock-source-face 'jcs-font-lock-source-face)

(defface jcs-font-lock-url-face
  '((t (:foreground "PaleTurquoise2" :underline t :weight bold)))
  "Highlight word 'URL'.")
(defvar jcs-font-lock-url-face 'jcs-font-lock-url-face)

(defface jcs-font-lock-idea-face
  '((t (:foreground "green yellow" :underline t :weight bold)))
  "Highlight word 'IDEA'.")
(defvar jcs-font-lock-idea-face 'jcs-font-lock-idea-face)

(defface jcs-font-lock-obsolete-face
  '((t (:foreground "DarkOrange3" :underline t :weight bold)))
  "Highlight word 'OBSOLETE'.")
(defvar jcs-font-lock-obsolete-face 'jcs-font-lock-obsolete-face)

(defface jcs-font-lock-deprecated-face
  '((t (:foreground "DarkOrange3" :underline t :weight bold)))
  "Highlight word 'DEPRECATED'.")
(defvar jcs-font-lock-deprecated-face 'jcs-font-lock-deprecated-face)

(defface jcs-font-lock-topic-face
  '((t (:foreground "slate blue" :underline t :weight bold)))
  "Highlight word 'TOPIC'.")
(defvar jcs-font-lock-topic-face 'jcs-font-lock-topic-face)

(defface jcs-font-lock-see-face
  '((t (:foreground "slate blue" :underline t :weight bold)))
  "Highlight word 'SEE'.")
(defvar jcs-font-lock-see-face 'jcs-font-lock-see-face)

(defface jcs-font-lock-option-face
  '((t (:foreground "dark green" :underline t :weight bold)))
  "Highlight word 'OPTION'.")
(defvar jcs-font-lock-option-face 'jcs-font-lock-option-face)

(defface jcs-font-lock-or-face
  '((t (:foreground "green yellow" :underline t :weight bold)))
  "Highlight word 'OR'.")
(defvar jcs-font-lock-or-face 'jcs-font-lock-or-face)

(defface jcs-font-lock-key-highlight-face
  '((t (:foreground "#38EFCA")))
  "Highlight word between ` and ' word.")
(defvar jcs-font-lock-key-highlight-face 'jcs-font-lock-key-highlight-face)


;;----------------------------------------------
;; Object Oriented Programming
;;----------------------------------------------

(defface jcs-oop-tag-face
  '((t (:foreground "#38EFCA")))
  "Highlight OOP tag.")
(defvar jcs-oop-tag-face 'jcs-oop-tag-face)

(defface jcs-oop-type-face
  '((t (:foreground "SteelBlue")))
  "Highlight OOP type.")
(defvar jcs-oop-type-face 'jcs-oop-type-face)

(defface jcs-oop-value-face
  '((t (:foreground "LightCoral")))
  "Highlight OOP value.")
(defvar jcs-oop-value-face 'jcs-oop-value-face)


;;----------------------------------------------
;; Python
;;----------------------------------------------

(defface jcs-py-mode-docstring-face
  '((t (:foreground "olive drab")))
  "Python mode docstring face.")
(defvar jcs-py-mode-docstring-face 'jcs-py-mode-docstring-face)


;;----------------------------------------------
;; Web
;;----------------------------------------------

(defface jcs-web-mode-block-comment-face
  '((t (:inherit 'jcs-font-lock-comment-face :background "#000000")))
  "Web mode block comment face with dark background."
  :group 'jcs-web-faces)
(defvar jcs-web-mode-block-comment-face 'jcs-web-mode-block-comment-face)

(defface jcs-web-mode-html-attr-value-face
  '((t (:foreground "olive drab")))
  "Highlight HTML value.")
(defvar jcs-web-mode-html-attr-value-face 'jcs-web-mode-html-attr-value-face)


(defface jcs-css-type-face
  '((t (:foreground "#38EFCA")))
  "Highlight CSS value.")
(defvar jcs-css-type-face 'jcs-css-type-face)

(defface jcs-css-value-face
  '((t (:foreground "#D2D2D2")))
  "Highlight CSS value.")
(defvar jcs-css-value-face 'jcs-css-value-face)

(defface jcs-css-id-face
  '((t (:foreground "#D68974")))
  "Highlight CSS id.")
(defvar jcs-css-id-face 'jcs-css-id-face)

(defface jcs-css-class-face
  '((t (:foreground "#FAD42D")))
  "Highlight CSS class.")
(defvar jcs-css-class-face 'jcs-css-class-face)

(defface jcs-css-event-face
  '((t (:foreground "#B592EA")))
  "Highlight CSS event.")
(defvar jcs-css-event-face 'jcs-css-event-face)

(defface jcs-css-number-face
  '((t (:foreground "#B5CE89")))
  "Highlight CSS number.")
(defvar jcs-css-number-face 'jcs-css-number-face)


;;----------------------------------------------
;; Load face order.
;;----------------------------------------------

;; Load OOP faces.
(when (fboundp 'jcs-oop-reload-faces)
  (call-interactively #'jcs-oop-reload-faces))

;; Fixme faces,
;; should always be the last.
(mapc (lambda (mode)
        (let ((case-fold-search t))
          (font-lock-add-keywords
           mode
           '(;; NOTE(jenchieh): Coding Use keywords.
             ("\\<\\(TODO\\)\\>" 1 'jcs-font-lock-fixme-face t)
             ("\\<\\(ATTENTION\\)\\>" 1 'jcs-font-lock-attention-face t)
             ("\\<\\(STUDY\\)\\>" 1 'jcs-font-lock-study-face t)
             ("\\<\\(CAUTION\\)\\>" 1 'jcs-font-lock-caution-face t)
             ("\\<\\(IMPORTANT\\)\\>" 1 'jcs-font-lock-important-face t)
             ("\\<\\(OPTIMIZE\\)\\>" 1 'jcs-font-lock-optimize-face t)
             ("\\<\\(NOTE\\)\\>" 1 'jcs-font-lock-note-face t)
             ("\\<\\(DESC\\)\\>" 1 'jcs-font-lock-description-face t)
             ("\\<\\(DESCRIPTION\\)\\>" 1 'jcs-font-lock-description-face t)
             ("\\<\\(TAG\\)\\>" 1 'jcs-font-lock-tag-face t)
             ("\\<\\(DEBUG\\)\\>" 1 'jcs-font-lock-debugging-face t)
             ("\\<\\(DEBUGGING\\)\\>" 1 'jcs-font-lock-debugging-face t)
             ("\\<\\(TEMP\\)\\>" 1 'jcs-font-lock-temporary-face t)
             ("\\<\\(TEMPORARY\\)\\>" 1 'jcs-font-lock-temporary-face t)
             ("\\<\\(SOURCE\\)\\>" 1 'jcs-font-lock-source-face t)
             ("\\<\\(URL\\)\\>" 1 'jcs-font-lock-url-face t)
             ("\\<\\(IDEA\\)\\>" 1 'jcs-font-lock-idea-face t)
             ("\\<\\(OBSOLETE\\)\\>" 1 'jcs-font-lock-obsolete-face t)
             ("\\<\\(DEPRECATED\\)\\>" 1 'jcs-font-lock-deprecated-face t)
             ("\\<\\(TOPIC\\)\\>" 1 'jcs-font-lock-topic-face t)
             ("\\<\\(SEE\\)\\>" 1 'jcs-font-lock-see-face t)

             ;; NOTE(jenchieh): Alternative keywords.
             ("\\<\\(OPTION\\)\\>" 1 'jcs-font-lock-option-face t)
             ("\\<\\(OR\\)\\>" 1 'jcs-font-lock-or-face t)

             ;; NOTE(jenchieh): Special keywords.
             ("`\\([a-zA-Z0-9_$-.!]*\\)'" 1 'jcs-font-lock-key-highlight-face t)
             )'end)))
      jcs-fixme-modes)


(provide 'jcs-face)
;;; jcs-face.el ends here
