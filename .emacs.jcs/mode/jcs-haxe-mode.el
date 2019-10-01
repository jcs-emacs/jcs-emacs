;;; jcs-haxe-mode.el --- Haxe mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'haxe-mode)


(defun jcs-haxe-mode-hook ()
  "Haxe mode hook."
  (abbrev-mode 1)
  (auto-highlight-symbol-mode t)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (highlight-indent-guides-mode 1)
  (lsp-deferred)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]hx")
                              'jcs-insert-haxe-template)

  ;; Normal
  (define-key haxe-mode-map (kbd "<up>") #'jcs-smart-indent-up)
  (define-key haxe-mode-map (kbd "<down>") #'jcs-smart-indent-down)

  (define-key haxe-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key haxe-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key haxe-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  (define-key haxe-mode-map (kbd "C-v") #'jcs-smart-yank)

  ;; switch frame.
  (define-key haxe-mode-map (kbd "M-w") #'jcs-other-window-next)
  (define-key haxe-mode-map (kbd "M-q") #'jcs-other-window-prev)
  )
(add-hook 'haxe-mode-hook 'jcs-haxe-mode-hook)


(provide 'jcs-haxe-mode)
;;; jcs-haxe-mode.el ends here
