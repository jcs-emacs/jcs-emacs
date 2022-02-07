;;; jcs-shell-mode.el --- Shell/Terminal mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'shell)
(require 'esh-mode)

(require 'exec-path-from-shell)

;; Fix issue from https://github.com/kyagi/shell-pop-el/issues/51
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

;;
;; (@* "Hook" )
;;

(jcs-add-hook '(shell-mode-hook eshell-mode-hook)
  (company-fuzzy-mode -1)

  (jcs-key-local
    `(((kbd "DEL")    . jcs-electric-backspace)
      ((kbd "{")      . jcs-vs-opening-curly-bracket-key)
      ((kbd ";")      . jcs-vs-semicolon-key)
      ((kbd "M-k")    . shell-pop)  ; Close it
      ((kbd "M-K")    . comint-clear-buffer)
      ((kbd "<up>")   . comint-previous-input)
      ((kbd "<down>") . comint-next-input)
      ((kbd "<tab>")  . comint-next-matching-input-from-input)
      ([C-up]         . jcs-previous-blank-line)
      ([C-down]       . jcs-next-blank-line)
      ((kbd "C-~")    . (lambda () (interactive)
                          (shell-pop (multi-shell--next-valid-index))))
      ((kbd "C-_")    . multi-shell-prev)
      ((kbd "C-+")    . multi-shell-next)
      ((kbd "M-b")    . multi-shell-select))))

(provide 'jcs-shell-mode)
;;; jcs-shell-mode.el ends here
