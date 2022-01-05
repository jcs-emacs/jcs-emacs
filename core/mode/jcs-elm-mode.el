;;; jcs-elm-mode.el --- Elm mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'elm-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-elm-template ()
  "Template for Elm."
  (jcs--file-header--insert "elm" "default.txt"))
;;
;; (@* "Hook" )
;;

(jcs-add-hook 'elm-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]elm") 'jcs-insert-elm-template))

(provide 'jcs-elm-mode)
;;; jcs-elm-mode.el ends here
