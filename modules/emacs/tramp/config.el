;;; emacs/tramp/config.el  -*- lexical-binding: t; -*-

(use-package tramp
  :init
  (setq tramp-default-method "ssh"
        tramp-verbose 1
        tramp-completion-reread-directory-timeout 60))
