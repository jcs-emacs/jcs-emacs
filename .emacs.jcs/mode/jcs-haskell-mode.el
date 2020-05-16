;;; jcs-haskell-mode.el --- Haskell mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'haskell-mode)

(defun jcs-haskell-mode-hook ()
  "Haskell mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]hs")
                              'jcs-insert-haskell-template))

(add-hook 'haskell-mode-hook 'jcs-haskell-mode-hook)

(provide 'jcs-haskell-mode)
;;; jcs-haskell-mode.el ends here
