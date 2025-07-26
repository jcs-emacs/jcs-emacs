;;; tools/editorconfig/config.el  -*- lexical-binding: t; -*-

(use-package editorconfig
  :init
  (setq editorconfig-trim-whitespaces-mode 'whitespace-cleanup-mode))

(use-package editorconfig-custom-majormode
  :init
  (add-hook 'editorconfig-after-apply-functions #'editorconfig-custom-majormode))
