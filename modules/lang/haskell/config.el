;;; lang/haskell/config.el  -*- lexical-binding: t; -*-

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

;;
;; (@* "Extensions" )
;;

(leaf flycheck-haskell
  :hook (flycheck-mode-hook . flycheck-haskell-setup))
