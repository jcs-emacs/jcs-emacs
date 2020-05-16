;;; jcs-make-mode.el --- Makefile mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'make-mode)

(defun jcs-makefile-mode-hook ()
  "Makefile mode hook."
  (electric-pair-mode nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]makefile"
                                "[Mm]akefile"
                                "[.]mak")
                              'jcs-makefile-format-info)

  ;; Normal
  (define-key makefile-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key makefile-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  (define-key makefile-mode-map (kbd "RET") #'jcs-makefile-newline)

  ;; tabify save key
  (define-key makefile-mode-map (kbd "C-s") #'jcs-tabify-save-buffer))

(add-hook 'makefile-mode-hook 'jcs-makefile-mode-hook)

(provide 'jcs-make-mode)
;;; jcs-make-mode.el ends here
