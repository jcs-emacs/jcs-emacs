;;; jcs-snippet-mode.el --- Snippet mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'yasnippet)


(defun jcs-snippet-mode-hook()
  "Snippet mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Normal
  (define-key snippet-mode-map (kbd "<up>") #'previous-line)
  (define-key snippet-mode-map (kbd "<down>") #'next-line)

  (define-key snippet-mode-map (kbd "C-s") #'jcs-save-buffer)

  (define-key snippet-mode-map (kbd "C-c C-c") #'kill-ring-save)
  )
(add-hook 'snippet-mode-hook 'jcs-snippet-mode-hook)


(provide 'jcs-snippet-mode)
;;; jcs-snippet-mode.el ends here
