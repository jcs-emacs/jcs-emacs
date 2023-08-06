;;; lang/ledger/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(use-package ledger-mode
  :init
  (setq ledger-clear-whole-transactions t
        ledger-mode-should-check-version nil))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-ledger
  :hook (flycheck-mode . (lambda (&rest _) (require 'flycheck-ledger))))
