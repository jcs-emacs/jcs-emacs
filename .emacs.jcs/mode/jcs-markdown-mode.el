;;; jcs-markdown-mode.el --- Markdown mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'markdown-mode)
(require 'web-mode)

(require 'jcs-markdown)
(require 'jcs-web)

(setq markdown-fontify-code-blocks-natively t)

(add-to-list 'markdown-code-lang-modes '("cs" . csharp-mode))
(add-to-list 'markdown-code-lang-modes '("el" . emacs-lisp-mode))
(add-to-list 'markdown-code-lang-modes '("cl" . lisp-mode))

;;
;; (@* "Hook" )
;;

(defun jcs-markdown-mode-hook ()
  "Markdown mode hook."
  (emojify-mode 1)

  (jcs-safe-er/expand-list '(web-mode-mark-and-expand) t)

  (jcs-make-electric-pair-pairs-local '((?\` . ?\`)))

  (jcs-company-safe-add-backend 'company-emojify)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]md" "[.]markdown")
                              'jcs-insert-markdown-template)

  ;; Normal
  (jcs-bind-key (kbd "<backspace>") #'jcs-real-backspace)
  (jcs-bind-key (kbd "RET") #'jcs-markdown-return-key)

  (jcs-bind-key [S-tab] #'markdown-cycle)

  ;; Eemmet
  (define-key emmet-mode-keymap (kbd "C-<return>") #'jcs-emmet-expand-line))

(add-hook 'markdown-mode-hook 'jcs-markdown-mode-hook)
(add-hook 'markdown-mode-hook 'emmet-mode)

(provide 'jcs-markdown-mode)
;;; jcs-markdown-mode.el ends here
