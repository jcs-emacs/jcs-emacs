;;; tools/prettier/config.el  -*- lexical-binding: t; -*-

(use-package prettier
  :config
  ;; XXX: Stop displaying the error when `prettier' is not installed!
  (unless (executable-find "prettier")
    (setq prettier-prettify-on-save-flag nil)))
