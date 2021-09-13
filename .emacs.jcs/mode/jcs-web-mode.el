;;; jcs-web-mode.el --- Web Development mode  -*- lexical-binding: t -*-
;;; Commentary: Including HTML, CSS, PHP, JavaScript, JSON.
;;; Code:

;;
;; (@* "Real time editing" )
;;

;; Note for "Impatient Mode" (real time editing)
;; Step 1: M-x httpd-start        (Open the port default: 8080)
;; Step 2: M-x impatient-mode     (Enabled Impatient Mode)

(require 'impatient-mode)

;;
;; (@* "Related Packages" )
;;

(require 'auto-rename-tag)
(require 'emmet-mode)
(require 'htmltagwrap)

;;
;; (@* "Hook" )
;;

(require 'web-mode)

(defun jcs-web-mode-hook ()
  "Hooks for Web mode."
  (auto-rename-tag-mode 1)
  (atl-markup-mode 1)
  (visual-line-mode t)
  (impatient-mode t)

  (jcs-enable-truncate-lines)

  ;; Docstring Faces
  (face-remap-add-relative 'docstr-faces-tag-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'docstr-faces-type-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'docstr-faces-value-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'web-mode-block-string-face '(font-lock-string-face))
  (face-remap-add-relative 'web-mode-html-attr-value-face '(jcs-web-mode-html-attr-value-face))

  (jcs-make-electric-pair-pairs-local '((?\' . ?\')))
  (jcs-make-electric-pair-pairs-local '((?\" . ?\")))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]htm" "[.]html"
                                "[.]asp"
                                "[.]as[cp]x")
                              'jcs-insert-html-template)
  (jcs-insert-header-if-valid '("[.]php")
                              'jcs-insert-php-template)

  ;; Normal
  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-web-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  ;; File Corresponding
  (jcs-bind-key [f8] #'jcs-find-corresponding-file)
  (jcs-bind-key [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; Shortcuts
  (jcs-bind-key (kbd "C-n") #'web-mode-tag-match)

  ;; Emmet
  (define-key emmet-mode-keymap (kbd "C-<return>") #'jcs-emmet-expand-line)

  ;; PHP
  (jcs-bind-key [C-backspace] #'jcs-web-backward-delete-word)
  (jcs-bind-key (kbd "M-<backspace>") #'jcs-web-backward-delete-word-capital))

(add-hook 'web-mode-hook 'jcs-web-mode-hook)
(add-hook 'web-mode-hook 'emmet-mode)

(defun jcs-html-mode-hook ()
  "HTML mode hook."
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(add-hook 'html-mode-hook 'jcs-html-mode-hook)

(provide 'jcs-web-mode)
;;; jcs-web-mode.el ends here
