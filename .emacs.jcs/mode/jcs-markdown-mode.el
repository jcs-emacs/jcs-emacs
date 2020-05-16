;;; jcs-markdown-mode.el --- Markdown mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'markdown-mode)

(require 'jcs-markdown-func)
(require 'jcs-web-func)

(defun jcs-markdown-mode-hook ()
  "Markdown mode hook."

  (jcs-make-electric-pair-pairs-local '((?\` . ?\`)))

  ;; Normal
  (define-key markdown-mode-map (kbd "<backspace>") #'jcs-real-backspace)
  (define-key markdown-mode-map (kbd "RET") #'jcs-markdown-return-key)

  (define-key markdown-mode-map (kbd "C-s") #'jcs-save-buffer)

  ;; Eemmet
  (define-key emmet-mode-keymap (kbd "C-<return>") #'jcs-emmet-expand-line))

(add-hook 'markdown-mode-hook 'jcs-markdown-mode-hook)
(add-hook 'markdown-mode-hook 'emmet-mode)

(provide 'jcs-markdown-mode)
;;; jcs-markdown-mode.el ends here
