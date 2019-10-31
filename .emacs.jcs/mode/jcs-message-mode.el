;;; jcs-message-mode.el --- Message mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'message)


(defun jcs-message-mode-hook ()
  "Message mode hook."
  (electric-pair-mode nil)

  ;; Normal
  (define-key message-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key message-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(add-hook 'message-mode-hook 'jcs-message-mode-hook)


(provide 'jcs-message-mode)
;;; jcs-message-mode.el ends here
