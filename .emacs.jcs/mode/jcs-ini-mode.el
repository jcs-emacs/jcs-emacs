;;; jcs-ini-mode.el --- INI mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'ini-mode)
(defun jcs-ini-mode-hook ()
  "INI mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")


  ;; Normal
  (define-key ini-mode-map (kbd "<up>") #'previous-line)
  (define-key ini-mode-map (kbd "<down>") #'next-line)

  (define-key ini-mode-map (kbd "C-s") #'save-buffer)

  )
(add-hook 'ini-mode-hook 'jcs-ini-mode-hook)

(add-to-list 'auto-mode-alist '("\\.properties'?\\'" . ini-mode))
(add-to-list 'auto-mode-alist '("\\.ini'?\\'" . ini-mode))


(provide 'jcs-ini-mode)
;;; jcs-ini-mode.el ends here
