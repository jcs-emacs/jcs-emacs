;;; jcs-elm-mode.el --- Elm mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'elm-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-elm-mode-hook ()
  "Elm mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]elm") 'jcs-insert-elm-template))

(add-hook 'elm-mode-hook 'jcs-elm-mode-hook)

(provide 'jcs-elm-mode)
;;; jcs-elm-mode.el ends here
