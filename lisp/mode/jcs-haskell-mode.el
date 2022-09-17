;;; jcs-haskell-mode.el --- Haskell mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'haskell-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-haskell-template "haskell" "default.txt"
  "Template for Haskell.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'haskell-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]hs")
                              'jcs-insert-haskell-template))

(provide 'jcs-haskell-mode)
;;; jcs-haskell-mode.el ends here
