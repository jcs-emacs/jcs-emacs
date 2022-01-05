;;; jcs-haskell-mode.el --- Haskell mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'haskell-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-haskell-template ()
  "Template for Haskell."
  (jcs--file-header--insert "haskell" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'haskell-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]hs")
                              'jcs-insert-haskell-template))

(provide 'jcs-haskell-mode)
;;; jcs-haskell-mode.el ends here
