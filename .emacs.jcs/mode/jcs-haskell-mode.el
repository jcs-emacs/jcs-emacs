;;; jcs-haskell-mode.el --- Haskell mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'haskell-mode)


(defun jcs-haskell-script-format ()
  "Format the given file as a Haskell file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-haskell-template)))


(defun jcs-haskell-mode-hook ()
  "Haskell mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]hs" buffer-file-name) (jcs-haskell-script-format))
          ))

  )
(add-hook 'haskell-mode-hook 'jcs-haskell-mode-hook)


(provide 'jcs-haskell-mode)
;;; jcs-haskell-mode.el ends here
