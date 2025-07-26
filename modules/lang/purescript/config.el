;;; lang/purescript/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-purescript-template "purescript" "default.txt"
  "Header for PureScript file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'purescript-mode-hook
  (company-fuzzy-backend-add-before 'company-psc-ide-backend 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]purs")
                              'jcs-insert-purescript-template))

;;
;; (@* "Extensions" )
;;

(use-package psc-ide
  :hook (purescript-mode . psc-ide-mode)
  :hook (flycheck-mode   . psc-ide-flycheck-setup))
