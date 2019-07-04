;;; jcs-elixir-mode.el --- Elixir Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'elixir-mode)


(defun jcs-elixir-mode-hook ()
  "Elixir mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")


  ;; File Header
  (jcs-insert-header-if-valid '("[.]ex"
                                "[.]exs")
                              'jcs-insert-elixir-template)

  )
(add-hook 'elixir-mode-hook 'jcs-elixir-mode-hook)


(provide 'jcs-elixir-mode)
;;; jcs-elixir-mode.el ends here
