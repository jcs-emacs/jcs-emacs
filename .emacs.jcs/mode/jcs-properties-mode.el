;;; jcs-properties-mode.el --- Properties mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-properties-mode-hook ()
  "Properties mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; Normal
  (define-key conf-javaprop-mode-map (kbd "<up>") #'previous-line)
  (define-key conf-javaprop-mode-map (kbd "<down>") #'next-line)

  (define-key conf-javaprop-mode-map (kbd "C-s") #'save-buffer)
  )
(add-hook 'conf-javaprop-mode-hook 'jcs-properties-mode-hook)


(provide 'jcs-properties-mode)
;;; jcs-properties-mode.el ends here
