;;; jcs-shell-mode.el --- Shell/Terminal mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'shell)


(defun jcs-shell-mode-hook ()
  "Shell mode hook."
  (company-fuzzy-mode -1)

  ;; Normal
  (define-key shell-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key shell-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key shell-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key shell-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  (define-key shell-mode-map (kbd "M-k") #'jcs-maybe-kill-shell)  ; Close it.

  ;; Completion
  (define-key shell-mode-map [tab] #'jcs-company-manual-begin)

  ;; Command Input
  (define-key shell-mode-map (kbd "RET") #'jcs-shell-return)

  ;; Navigation
  (define-key shell-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key shell-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  (define-key shell-mode-map [C-up] #'jcs-previous-blank-line)
  (define-key shell-mode-map [C-down] #'jcs-next-blank-line)

  (define-key shell-mode-map (kbd "C-~") #'jcs-shell-new-shell)

  (define-key shell-mode-map (kbd "C-_") #'multi-shell-prev)
  (define-key shell-mode-map (kbd "C-+") #'multi-shell-next)

  ;; Deletion
  (define-key shell-mode-map (kbd "C-<backspace>") #'jcs-shell-backward-delete-word)
  (define-key shell-mode-map (kbd "C-S-<backspace>") #'jcs-shell-forward-delete-word)
  (define-key shell-mode-map (kbd "M-<backspace>") #'jcs-shell-backward-kill-word-capital)
  (define-key shell-mode-map (kbd "M-S-<backspace>") #'jcs-shell-forward-kill-word-capital)

  (define-key shell-mode-map (kbd "C-d") #'jcs-shell-kill-whole-line)
  (define-key shell-mode-map (kbd "<backspace>") #'jcs-shell-backspace)

  ;; Mode Line
  (define-key shell-mode-map (kbd "C-M-m") #'feebleline-mode))

(add-hook 'shell-mode-hook #'jcs-shell-mode-hook)


(provide 'jcs-shell-mode)
;;; jcs-shell-mode.el ends here
