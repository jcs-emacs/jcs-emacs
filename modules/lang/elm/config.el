;;; lang/elm/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-elm-template "elm" "default.txt"
  "Template for Elm.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'elm-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]elm") 'jcs-insert-elm-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-elm
  :hook (flycheck-mode . flycheck-elm-setup))
