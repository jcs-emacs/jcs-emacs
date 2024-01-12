;;; editor/vs/config.el  -*- lexical-binding: t; -*-

(use-package vsc-edit-mode
  :init
  (setq vsc-edit-insert-tab-on-tab t)

  (message-clean-mode-add-echo-commands
   '( vsc-edit-beginning-of-line vsc-edit-end-of-line)))

(use-package vs-comment-return
  :init
  (setq vs-comment-return-cancel-after t
        vs-comment-return-inhibit-prefix nil))
