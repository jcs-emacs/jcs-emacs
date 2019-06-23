;;; jcs-haxe-mode.el --- Haxe mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'haxe-mode)


(defun jcs-haxe-mode-hook ()
  "Haxe mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]hx" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-haxe-template))
          ))

  ;; Normal
  (define-key haxe-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key haxe-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key haxe-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key haxe-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key haxe-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; switch frame.
  (define-key haxe-mode-map "\ew" #'jcs-other-window-next)
  (define-key haxe-mode-map (kbd "M-q") #'jcs-other-window-prev)
  )
(add-hook 'haxe-mode-hook 'jcs-haxe-mode-hook)


(provide 'jcs-haxe-mode)
;;; jcs-haxe-mode.el ends here
