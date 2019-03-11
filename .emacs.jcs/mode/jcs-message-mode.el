;;; jcs-message-mode.el --- Message mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-message-mode-hook ()
  "Message mode hook."

  (abbrev-mode 1)

  (electric-pair-mode nil)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (define-key message-mode-map (kbd "<up>") #'previous-line)
  (define-key message-mode-map (kbd "<down>") #'next-line)
  )
(add-hook 'message-mode-hook 'jcs-message-mode-hook)


(provide 'jcs-message-mode)
;;; jcs-message-mode.el ends here
