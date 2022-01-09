;;; jcs-shell-mode.el --- Shell/Terminal mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'shell)
(require 'esh-mode)

;;
;; (@* "Hook" )
;;

(jcs-add-hook '(shell-mode-hook eshell-mode-hook)
  (company-fuzzy-mode -1)

  (jcs-key-local
    `(((kbd "DEL") . jcs-electric-backspace)
      ((kbd "{") . jcs-vs-opening-curly-bracket-key)
      ((kbd ";") . jcs-vs-semicolon-key)
      ((kbd "M-k") . jcs-maybe-kill-shell)  ; Close it

      ([tab]       . jcs-company-manual-begin)
      ((kbd "RET") . jcs-shell-return)

      ;; Navigation
      ((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ([C-up]         . jcs-previous-blank-line)
      ([C-down]       . jcs-next-blank-line)

      ((kbd "C-~") . jcs-shell-new-shell)

      ((kbd "C-_") . multi-shell-prev)
      ((kbd "C-+") . multi-shell-next)

      ;; Deletion
      ((kbd "C-<backspace>")   . jcs-shell-backward-delete-word)
      ((kbd "C-S-<backspace>") . jcs-shell-forward-delete-word)
      ((kbd "M-<backspace>")   . jcs-shell-backward-kill-word-capital)
      ((kbd "M-S-<backspace>") . jcs-shell-forward-kill-word-capital)

      ((kbd "C-d")         . jcs-shell-kill-whole-line)
      ((kbd "<backspace>") . jcs-shell-backspace))))

(provide 'jcs-shell-mode)
;;; jcs-shell-mode.el ends here
