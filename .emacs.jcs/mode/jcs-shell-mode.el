;;; jcs-shell-mode.el --- Shell/Terminal mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'shell)
(require 'esh-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-shell-mode-hook ()
  "Shell mode hook."
  (company-fuzzy-mode -1)

  ;; Normal
  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  (jcs-bind-key (kbd "M-k") #'jcs-maybe-kill-shell)  ; Close it.

  ;; Completion
  (jcs-bind-key [tab] #'jcs-company-manual-begin)

  ;; Command Input
  (jcs-bind-key (kbd "RET") #'jcs-shell-return)

  ;; Navigation
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  (jcs-bind-key [C-up] #'jcs-previous-blank-line)
  (jcs-bind-key [C-down] #'jcs-next-blank-line)

  (jcs-bind-key (kbd "C-~") #'jcs-shell-new-shell)

  (jcs-bind-key (kbd "C-_") #'multi-shell-prev)
  (jcs-bind-key (kbd "C-+") #'multi-shell-next)

  ;; Deletion
  (jcs-bind-key (kbd "C-<backspace>") #'jcs-shell-backward-delete-word)
  (jcs-bind-key (kbd "C-S-<backspace>") #'jcs-shell-forward-delete-word)
  (jcs-bind-key (kbd "M-<backspace>") #'jcs-shell-backward-kill-word-capital)
  (jcs-bind-key (kbd "M-S-<backspace>") #'jcs-shell-forward-kill-word-capital)

  (jcs-bind-key (kbd "C-d") #'jcs-shell-kill-whole-line)
  (jcs-bind-key (kbd "<backspace>") #'jcs-shell-backspace)

  ;; Mode Line
  (jcs-bind-key (kbd "C-M-m") #'feebleline-mode))

(add-hook 'shell-mode-hook #'jcs-shell-mode-hook)
(add-hook 'eshell-mode-hook 'jcs-shell-mode-hook)

(provide 'jcs-shell-mode)
;;; jcs-shell-mode.el ends here
