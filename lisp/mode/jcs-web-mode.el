;;; jcs-web-mode.el --- Web Development mode  -*- lexical-binding: t -*-
;;; Commentary: Including HTML, CSS, PHP, JavaScript, JSON.
;;; Code:

(require 'web-mode)

(require 'jcs-web)
(require 'impatient-mode)
(require 'auto-rename-tag)
(require 'emmet-mode)
(require 'htmltagwrap)

;;
;; (@* "Deletion" )
;;

(defun jcs-web-backward-delete-word ()
  "Web backward delete the word, fit PHP variable naming."
  (interactive)
  (backward-delete-char 1)
  (when (and (not (jcs-current-whitespace-or-tab-p))
             (not (jcs-current-char-equal-p "$"))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word)))

(defun jcs-web-backward-delete-word-capital ()
  "Web backward delete word capital, fit PHP variable naming."
  (interactive)
  (backward-delete-char 1)
  (when (and (not (jcs-current-whitespace-or-tab-p))
             (not (jcs-current-char-equal-p "$"))
             (not (jcs-current-char-uppercasep))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word-capital))
  (when (and (jcs-current-char-uppercasep)
             (not (jcs-current-char-equal-p "$")))
    (backward-delete-char 1)))

;;
;; (@* "Indentation" )
;;

(defun jcs-web-vs-opening-curly-bracket-key ()
  "Web mode front curly bracket key."
  (interactive)
  (if (jcs-current-point-face '(web-mode-script-face
                                web-mode-block-face
                                web-mode-style-face))
      (call-interactively #'jcs-vs-opening-curly-bracket-key)
    (insert "{}")
    (backward-char 1)))

;;
;; (@* "Faces" )
;;

(defun jcs-init-web-faces ()
  "Initialize Web mode faces highlihgting."
  (let ((missing-modes '(web-mode)))
    (dolist (mode missing-modes)
      (font-lock-add-keywords
       mode
       '(;; For nomral HTML comment.
         ("\\(<!--[a-zA-Z0-9 \n\t-.<>?,*'`@\"=_(){}:;&^%$#!~]*-->\\)" 1 'font-lock-comment-face t)
         ("\\(@[ \t\n]*{[[:ascii:]]*\\)/\\*[[:ascii:]]*\\*/[[:ascii:]]*}" 1 'jcs-web-mode-block-face t)
         ("@[ \t\n]*{[[:ascii:]]*/\\*[[:ascii:]]*\\*/\\([[:ascii:]]*}\\)" 1 'jcs-web-mode-block-face t)
         ;; For multi-lines comment.
         ("@[ \t\n]*{[[:ascii:]]*\\(/\\*[[:ascii:]]*\\*/\\)[[:ascii:]]*}" 1 'jcs-web-mode-block-comment-face t))
       'end)))
  (set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")
  (set-face-attribute 'web-mode-block-comment-face nil :foreground (face-foreground font-lock-comment-face))
  (set-face-attribute 'web-mode-comment-face nil :foreground (face-foreground font-lock-comment-face)))

(jcs-init-web-faces)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-html-template ()
  "Template for HTML."
  (jcs--file-header--insert "web" "default_html.txt"))

(defun jcs-insert-php-template ()
  "Template for PHP."
  (jcs--file-header--insert "web" "default_php.txt"))

;;
;; (@* "Hook" )
;;

(add-hook 'web-mode-hook 'emmet-mode)

(jcs-add-hook 'web-mode-hook
  (auto-rename-tag-mode 1)
  (visual-line-mode t)
  (impatient-mode t)
  (toggle-truncate-lines 1)

  ;; Docstring Faces
  (face-remap-add-relative 'docstr-faces-tag-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'docstr-faces-type-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'docstr-faces-value-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'web-mode-block-string-face '(font-lock-string-face))
  (face-remap-add-relative 'web-mode-html-attr-value-face '(jcs-web-mode-html-attr-value-face))

  (jcs-elec-pair-add '((?\' . ?\')))
  (jcs-elec-pair-add '((?\" . ?\")))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]htm" "[.]html"
                                "[.]asp"
                                "[.]as[cp]x")
                              'jcs-insert-html-template)
  (jcs-insert-header-if-valid '("[.]php")
                              'jcs-insert-php-template)

  (jcs-key-local
    `(((kbd "DEL") . jcs-electric-backspace)
      ((kbd "{")   . jcs-web-vs-opening-curly-bracket-key)
      ((kbd ";")   . jcs-vs-semicolon-key)

      ;; File Corresponding
      ([f8]   . jcs-find-corresponding-file)
      ([S-f8] . jcs-find-corresponding-file-other-window)

      ;; Shortcuts
      ((kbd "C-n") . web-mode-tag-match)

      ;; PHP
      ([C-backspace]         . jcs-web-backward-delete-word)
      ((kbd "M-<backspace>") . jcs-web-backward-delete-word-capital)))

  ;; Emmet
  (jcs-key emmet-mode-keymap
    `((kbd "C-<return>") . jcs-emmet-expand-line)))

(jcs-add-hook 'html-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next)))))

(provide 'jcs-web-mode)
;;; jcs-web-mode.el ends here
