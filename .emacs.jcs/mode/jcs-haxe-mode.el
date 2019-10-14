;;; jcs-haxe-mode.el --- Haxe mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'haxe-mode)


(defun jcs-haxe-mode-hook ()
  "Haxe mode hook."

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]hx")
                              'jcs-insert-haxe-template)

  ;; Normal
  (define-key haxe-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key haxe-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (define-key haxe-mode-map (kbd "<backspace>") #'jcs-smart-backspace)
  (define-key haxe-mode-map (kbd "<delete>") #'jcs-smart-delete)

  (define-key haxe-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key haxe-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key haxe-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key haxe-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  (define-key haxe-mode-map (kbd "C-v") #'jcs-smart-yank)

  ;; switch frame.
  (define-key haxe-mode-map (kbd "M-w") #'jcs-other-window-next)
  (define-key haxe-mode-map (kbd "M-q") #'jcs-other-window-prev)
  )
(add-hook 'haxe-mode-hook 'jcs-prog-mode-hook)
(add-hook 'haxe-mode-hook 'jcs-haxe-mode-hook)


(provide 'jcs-haxe-mode)
;;; jcs-haxe-mode.el ends here
