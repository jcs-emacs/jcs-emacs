;;; jcs-asm-mode.el --- Assembly Language Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'masm-mode)
(require 'nasm-mode)

(require 'jcs-python-func)

(defun jcs-asm-mode--inti ()
  "Do insert file header and switch major mode picked."
    (when (and (not jcs-asm--asking-mode)
               ;; Insert file header.
               (not (jcs-insert-header-if-valid '("[.]asm"
                                                  "[.]inc")
                                                'jcs-asm-ask-source
                                                t)))
      ;; Switch major mode.
      (let ((jcs-asm--asking-mode t)) (call-interactively #'jcs-asm-ask-mode))))

(defun jcs-masm-mode-hook ()
  "MASM mode hook."
  (electric-pair-mode nil)
  (modify-syntax-entry ?_ "w")

  (jcs-asm-mode--inti)

  ;; Normal
  (define-key masm-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key masm-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  ;; Comment
  (define-key masm-mode-map (kbd "RET") #'jcs-asm-return)
  (define-key masm-mode-map (kbd ";") #'jcs-asm-comment))

(add-hook 'masm-mode-hook 'jcs-masm-mode-hook)


(defun jcs-nasm-mode-hook ()
  "NASM mode hook."
  (electric-pair-mode nil)

  (modify-syntax-entry ?_ "w")

  (jcs-asm-mode--inti)

  ;; Normal
  (define-key nasm-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key nasm-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  ;; Comment
  (define-key nasm-mode-map (kbd "RET") #'jcs-asm-return)
  (define-key nasm-mode-map (kbd ";") #'jcs-asm-comment))

(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)


(provide 'jcs-asm-mode)
;;; jcs-asm-mode.el ends here
