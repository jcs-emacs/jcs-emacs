;;; jcs-markdown-mode.el --- Markdown mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'markdown-mode)


(defun jcs-markdown-mode-hook ()
  "Markdown mode hook."

  (jcs-make-electric-pair-pairs-local '((?\` . ?\`)))


  ;; Normal
  (define-key markdown-mode-map (kbd "<backspace>") #'jcs-real-backspace)

  (define-key markdown-mode-map (kbd "C-s") #'save-buffer)
  )
(add-hook 'markdown-mode-hook 'jcs-markdown-mode-hook)


(provide 'jcs-markdown-mode)
;;; jcs-markdown-mode.el ends here
