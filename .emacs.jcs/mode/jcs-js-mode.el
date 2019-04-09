;;; jcs-js-mode.el --- JavaScript mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package skewer-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))


(require 'js2-mode)
(defun jcs-js-mode-hook ()
  "JavaScript mode hook."
  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)

  (impatient-mode t)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)
  (auto-complete-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-javascript-format()
    "JavaScript template format."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-js-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]js" buffer-file-name) (jcs-javascript-format))
          ))

  ;; Set Faces.
  (jcs-init-set-face)

  ;; Normal
  (define-key js2-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key js2-mode-map "\C-c\C-c" #'kill-ring-save)
  (define-key ac-js2-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key js2-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key js2-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key js2-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key js2-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key js2-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'js2-mode-hook 'jcs-js-mode-hook)

(add-to-list 'auto-mode-alist '("\\.js'?\\'" . js2-mode))


(provide 'jcs-js-mode)
;;; jcs-js-mode.el ends here
