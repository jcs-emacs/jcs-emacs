;;; jcs-nasm-mode.el --- Assembly Language Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'nasm-mode)

(require 'jcs-python-func)


(defun jcs-nasm-mode-hook ()
  "NASM mode hook."
  (electric-pair-mode nil)
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]asm"
                                "[.]inc")
                              'jcs-insert-asm-template)

  ;; Normal
  (define-key nasm-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key nasm-mode-map (kbd "C-c C-c") #'kill-ring-save)
  (define-key nasm-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key nasm-mode-map (kbd "<down>") #'jcs-py-indent-down)

  ;; Comment
  (define-key nasm-mode-map (kbd "RET") #'jcs-nasm-return)
  (define-key nasm-mode-map (kbd ";") #'jcs-nasm-comment)

  ;; Edit
  (define-key nasm-mode-map (kbd "<backspace>") #'jcs-py-backspace)
  )
(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)


(provide 'jcs-nasm-mode)
;;; jcs-nasm-mode.el ends here
