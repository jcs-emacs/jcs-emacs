;;; jcs-elm-mode.el --- Elm mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'elm-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-elm-template "elm" "default.txt"
  "Template for Elm.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'elm-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]elm") 'jcs-insert-elm-template))

;;
;; (@* "Extensions" )
;;

(leaf flycheck-elm
  :hook (flycheck-mode-hook . flycheck-elm-setup))

(provide 'jcs-elm-mode)
;;; jcs-elm-mode.el ends here
