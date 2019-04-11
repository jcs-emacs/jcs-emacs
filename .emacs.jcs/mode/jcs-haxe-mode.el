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

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")

  (defun jcs-haxe-script-format ()
    "Format the given file as a Haxe file"
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-haxe-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]hx" buffer-file-name) (jcs-haxe-script-format))
          ))

  ;; Normal
  (define-key haxe-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key haxe-mode-map "\C-c\C-c" #'kill-ring-save)

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
