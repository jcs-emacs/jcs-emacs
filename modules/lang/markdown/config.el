;;; lang/markdown/config.el  -*- lexical-binding: t; -*-

(require 'web-mode)
(require 'expand-region)

;;
;; (@* "Settings" )
;;

(setq markdown-enable-math t ; syntax highlighting for latex fragments
      markdown-enable-wiki-links t
      markdown-italic-underscore t
      markdown-asymmetric-header t
      markdown-gfm-additional-languages '("sh")
      markdown-make-gfm-checkboxes-buttons t
      markdown-fontify-code-blocks-natively t
      markdown-fontify-whole-heading-line t)

(elenv-uappend markdown-code-lang-modes
  '(("cs" . csharp-mode)
    ("el" . emacs-lisp-mode)
    ("cl" . lisp-mode)))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-markdown-template "markdown" "default.txt"
  "Header for Markdown header file.")

;;
;; (@* "Hook" )
;;

(add-hook 'markdown-mode-hook 'emmet-mode)

(jcs-add-hook 'markdown-mode-hook
  (jcs-elec-pair-add '((?\` . ?\`)))

  (company-fuzzy-backend-add-before 'company-emojify 'company-dabbrev)
  (jcs-safe-er/expand-list '(web-mode-mark-and-expand) t)

  (emojify-mode 1)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]md" "[.]markdown")
                              'jcs-insert-markdown-template)

  (jcs-key-local
    `(([S-tab] . markdown-cycle)))

  ;; Eemmet
  (jcs-key emmet-mode-keymap
    `(((kbd "C-<return>") . jcs-emmet-expand-line))))

;;
;; (@* "Extensions" )
;;

(use-package markdown-toc
  :init
  (setq markdown-toc-indentation-space 2))

(use-package impatient-showdown
  :init
  (setq impatient-showdown-flavor 'github))

(use-package flymake-markdownlint
  :hook (flymake-mode . flymake-markdownlint-setup))
