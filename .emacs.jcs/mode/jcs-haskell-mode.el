;;; jcs-haskell-mode.el --- Haskell mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'haskell-mode)


(defun jcs-haskell-mode-hook ()
  "Haskell mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]hs" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-haskell-template))
          ))

  )
(add-hook 'haskell-mode-hook 'jcs-haskell-mode-hook)


(provide 'jcs-haskell-mode)
;;; jcs-haskell-mode.el ends here
