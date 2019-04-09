;;; jcs-markdown-mode.el --- Markdown mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'markdown-mode)
(defun jcs-markdown-mode-hook ()
  "Markdown mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)

  ;; Normal
  (define-key markdown-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key markdown-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key markdown-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key markdown-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'markdown-mode-hook 'jcs-markdown-mode-hook)

(add-to-list 'auto-mode-alist '("\\.md'?\\'" . markdown-mode))


(provide 'jcs-markdown-mode)
;;; jcs-markdown-mode.el ends here
