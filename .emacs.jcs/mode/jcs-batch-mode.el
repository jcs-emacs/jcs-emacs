;;; jcs-batch-mode.el --- Batch Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'bat-mode)


(defun jcs-batch-mode-hook ()
  "Batch mode hook."

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]bat")
                              'jcs-insert-batch-template)

  ;; Normal
  (define-key bat-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key bat-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  )
(add-hook 'bat-mode-hook 'jcs-batch-mode-hook)


(provide 'jcs-batch-mode)
;;; jcs-batch-mode.el ends here
