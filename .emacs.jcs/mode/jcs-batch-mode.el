;;; jcs-batch-mode.el --- Batch Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'bat-mode)


(defun jcs-batch-mode-hook ()
  "Batch mode hook."
  (electric-pair-mode nil)
  (abbrev-mode 1)
  (goto-address-mode 1)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]bat")
                              'jcs-insert-batch-template)

  ;; Normal
  (define-key bat-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key bat-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key bat-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key bat-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'bat-mode-hook 'jcs-batch-mode-hook)


(provide 'jcs-batch-mode)
;;; jcs-batch-mode.el ends here
