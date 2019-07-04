;;; jcs-go-mode.el --- GO mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'go-mode)


(defun jcs-go-mode-hook ()
  "Go mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]go")
                              'jcs-insert-go-template)

  ;; Normal
  (define-key go-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key go-mode-map (kbd "C-c C-c") #'kill-ring-save)
  )
(add-hook 'go-mode-hook 'jcs-go-mode-hook)


(provide 'jcs-go-mode)
;;; jcs-go-mode.el ends here
