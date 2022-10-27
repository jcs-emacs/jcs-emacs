;;; term/eshell/config.el  -*- lexical-binding: t; -*-

(jcs-module-load "term/shell")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'eshell-mode-hook
  (modify-syntax-entry ?> "!")

  (jcs-key-local
    `(((kbd "M-k")    . shell-pop)  ; Close it
      ((kbd "M-K")    . comint-clear-buffer)
      ((kbd "<up>")   . comint-previous-input)
      ((kbd "<down>") . comint-next-input)
      ((kbd "<tab>")  . comint-next-matching-input-from-input)
      ([C-up]         . block-travel-up)
      ([C-down]       . block-travel-down)
      ((kbd "C-~")    . (lambda () (interactive)
                          (shell-pop (multi-shell--next-valid-index))))
      ((kbd "C-_")    . multi-shell-prev)
      ((kbd "C-+")    . multi-shell-next)
      ((kbd "M-b")    . multi-shell-select))))

;;
;; (@* "Extensions" )
;;

(leaf eshell-syntax-highlighting
  :hook (eshell-mode-hook . eshell-syntax-highlighting-global-mode))
