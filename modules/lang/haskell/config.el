;;; lang/haskell/config.el  -*- lexical-binding: t; -*-

(require 'haskell-cabal)

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

(jcs-add-hook 'haskell-cabal-mode-hook
  (run-hooks 'prog-mode-hook)

  (company-fuzzy-backend-add-before 'company-cabal 'company-dabbrev))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-haskell
  :hook (flycheck-mode . flycheck-haskell-setup))
