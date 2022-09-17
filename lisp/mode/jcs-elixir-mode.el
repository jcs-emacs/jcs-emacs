;;; jcs-elixir-mode.el --- Elixir mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'elixir-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-elixir-template "elixir" "default.txt"
  "Template for Elixir.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'elixir-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")


  ;; File Header
  (jcs-insert-header-if-valid '("[.]ex"
                                "[.]exs")
                              'jcs-insert-elixir-template))

(provide 'jcs-elixir-mode)
;;; jcs-elixir-mode.el ends here
