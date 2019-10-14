;;; jcs-nasm-mode.el --- Assembly Language Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'nasm-mode)

(require 'jcs-python-func)


(defun jcs-nasm-mode-hook ()
  "NASM mode hook."
  (electric-pair-mode nil)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]asm"
                                "[.]inc")
                              'jcs-insert-asm-template)

  ;; Normal
  (define-key nasm-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key nasm-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  ;; Comment
  (define-key nasm-mode-map (kbd "RET") #'jcs-nasm-return)
  (define-key nasm-mode-map (kbd ";") #'jcs-nasm-comment)
  )
(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)


(provide 'jcs-nasm-mode)
;;; jcs-nasm-mode.el ends here
