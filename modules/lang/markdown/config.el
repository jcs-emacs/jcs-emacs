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

(nconc markdown-code-lang-modes'(("cs" . csharp-mode)
                                 ("el" . emacs-lisp-mode)
                                 ("cl" . lisp-mode)))

;;
;; (@* "Keys" )
;;

(defun jcs-markdown-return-key ()
  "Return key for Markdown mode."
  (interactive)
  (let (did-ret-key close-tag-found)
    (when (and (jcs-first-forward-char-in-line-p "<")
               (jcs-first-backward-char-in-line-p ">"))
      ;; Check closing tag.
      (save-excursion
        (search-forward "<" nil t)
        (forward-char 1)
        (setq close-tag-found (jcs-current-char-equal-p "/")))

      (when close-tag-found
        (newline-and-indent)
        (newline-and-indent)
        (jcs-smart-previous-line)
        (setq did-ret-key t)))

    (unless did-ret-key
      (newline)
      (indent-for-tab-command))))

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

  (company-fuzzy-backend-add 'company-emojify)
  (jcs-safe-er/expand-list '(web-mode-mark-and-expand) t)

  (emojify-mode 1)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]md" "[.]markdown")
                              'jcs-insert-markdown-template)

  (jcs-key-local
    `(((kbd "RET") . jcs-markdown-return-key)
      ([S-tab]     . markdown-cycle)))

  ;; Eemmet
  (jcs-key emmet-mode-keymap
    `(((kbd "C-<return>") . jcs-emmet-expand-line))))

;;
;; (@* "Extensions" )
;;

(leaf markdown-toc
  :init
  (setq markdown-toc-indentation-space 2))

(leaf impatient-showdown
  :init
  (setq impatient-showdown-flavor 'github))
