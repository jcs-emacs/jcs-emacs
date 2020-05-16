;;; jcs-ini-mode.el --- INI mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ini-mode)

(defun jcs-ini-mode-hook ()
  "INI mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")


  ;; Normal
  (define-key ini-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key ini-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (define-key ini-mode-map (kbd "C-s") #'save-buffer))

(add-hook 'ini-mode-hook 'jcs-ini-mode-hook)

(provide 'jcs-ini-mode)
;;; jcs-ini-mode.el ends here
