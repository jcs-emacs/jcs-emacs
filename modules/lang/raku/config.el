;;; lang/raku/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-raku-template "raku" "default.txt"
  "Header for Raku.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'raku-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]raku")
                              'jcs-insert-raku-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-raku
  :hook (flycheck-mode . (lambda (&rest _) (require 'flycheck-raku))))
