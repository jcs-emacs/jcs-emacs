;;; jcs-asm-mode.el --- Assembly Language related modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'masm-mode)
(require 'nasm-mode)

(require 'jcs-python)

(defun jcs-asm-mode--init ()
  "Do insert file header and switch major mode picked."
  (when (and (not jcs-asm--asking-mode)
             ;; Insert file header.
             (not (jcs-insert-header-if-valid '("[.]asm"
                                                "[.]inc")
                                              'jcs-asm-ask-source
                                              :interactive t)))
    ;; Switch major mode.
    (let ((jcs-asm--asking-mode t)) (call-interactively #'jcs-asm-ask-mode))))

;;
;; (@* "Hook" )
;;

(defun jcs-masm-mode-hook ()
  "MASM mode hook."
  (electric-pair-mode nil)
  (modify-syntax-entry ?_ "w")

  (jcs-asm-mode--init)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  ;; Comment
  (jcs-bind-key (kbd "RET") #'jcs-asm-return)
  (jcs-bind-key (kbd ";") #'jcs-asm-comment))

(add-hook 'masm-mode-hook 'jcs-masm-mode-hook)

(defun jcs-nasm-mode-hook ()
  "NASM mode hook."
  (electric-pair-mode nil)

  (modify-syntax-entry ?_ "w")

  (jcs-asm-mode--init)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  ;; Comment
  (jcs-bind-key (kbd "RET") #'jcs-asm-return)
  (jcs-bind-key (kbd ";") #'jcs-asm-comment))

(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)

(provide 'jcs-asm-mode)
;;; jcs-asm-mode.el ends here
