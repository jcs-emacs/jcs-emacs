;;; jcs-cobol-mode.el --- COBOL mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'cobol-mode)

(require 'jcs-python-func)


(defun jcs-cobol-mode-hook ()
  "COBOL mode hook."
  (electric-pair-mode nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cbl")
                              'jcs-insert-cobol-template)

  ;; Normal
  (define-key cobol-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key cobol-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  )
(add-hook 'cobol-mode-hook 'jcs-cobol-mode-hook)


(provide 'jcs-cobol-mode)
;;; jcs-cobol-mode.el ends here
