;;; jcs-go-mode.el --- GO mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'go-mode)

(defun jcs-go-mode-hook ()
  "Go mode hook."

  (jcs-use-cc-mutliline-comment)

  (face-remap-add-relative 'font-lock-constant-face '(:inherit jcs-font-lock-null-face))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]go")
                              'jcs-insert-go-template)

  ;; comment block
  (define-key go-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key go-mode-map (kbd "*") #'jcs-c-comment-pair)

  (define-key go-mode-map (kbd "/") #'jcs-go-maybe-insert-codedoc))

(add-hook 'go-mode-hook 'jcs-go-mode-hook)

(provide 'jcs-go-mode)
;;; jcs-go-mode.el ends here
