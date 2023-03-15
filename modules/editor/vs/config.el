;;; editor/vs/config.el  -*- lexical-binding: t; -*-

(use-package vsc-edit-mode
  :init
  (setq vsc-edit-insert-tab-on-tab t))

(use-package vs-comment-return
  :init
  (setq vs-comment-return-cancel-after t))
