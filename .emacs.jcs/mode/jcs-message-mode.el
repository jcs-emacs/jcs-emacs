;;; jcs-message-mode.el --- Message mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'message)


(defun jcs-message-mode-hook ()
  "Message mode hook."
  (electric-pair-mode nil)

  ;; Normal
  (define-key message-mode-map (kbd "<up>") #'previous-line)
  (define-key message-mode-map (kbd "<down>") #'next-line)
  )
(add-hook 'message-mode-hook 'jcs-message-mode-hook)


(provide 'jcs-message-mode)
;;; jcs-message-mode.el ends here
