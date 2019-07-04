;;; jcs-erlang-mode.el --- Erlang Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'erlang)


(defun jcs-erlang-mode-hook ()
  "Erlang mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]erl"
                                "[.]hrl")
                              'jcs-insert-erlang-template)

  )
(add-hook 'erlang-mode-hook 'jcs-erlang-mode-hook)


(provide 'jcs-erlang-mode)
;;; jcs-erlang-mode.el ends here
