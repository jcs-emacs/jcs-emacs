;;; jcs-markdown-mode.el --- Markdown mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'markdown-mode)
(require 'web-mode)

(setq markdown-fontify-code-blocks-natively t)

(nconc markdown-code-lang-modes'(("cs" . csharp-mode)
                                 ("el" . emacs-lisp-mode)
                                 ("cl" . lisp-mode)))

(defun jcs-markdown-return-key ()
  "Return key for Markdown mode."
  (interactive)
  (let (did-ret-key close-tag-found)
    (when (and (jcs-first-forward-char-in-line-p "<")
               (jcs-first-backward-char-in-line-p ">"))
      ;; Check closing tag.
      (save-excursion
        (jcs-move-to-forward-a-char "<")
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
  (modify-syntax-entry ?: "w")
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


(provide 'jcs-markdown-mode)
;;; jcs-markdown-mode.el ends here
