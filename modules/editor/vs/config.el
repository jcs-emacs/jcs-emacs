;;; editor/vs/config.el  -*- lexical-binding: t; -*-

(use-package vs-edit-mode
  :init
  (eldoc-add-command
   'vs-edit-previous-line 'vs-edit-next-line
   'vs-edit-forward-word 'vs-edit-backward-word))

(use-package vsc-edit-mode
  :init
  (setq vsc-edit-insert-tab-on-tab t)

  (msg-clean-add-echo-commands
   '( vsc-edit-beginning-of-line vsc-edit-end-of-line))

  (eldoc-add-command
   'vsc-edit-real-space 'vsc-edit-smart-space 'vsc-edit-space
   'vsc-edit-real-backspace 'vsc-edit-smart-backspace 'vsc-edit-backspace
   'vsc-edit-beginning-of-line 'vsc-edit-end-of-line))

(use-package vs-comment-return
  :init
  (setq vs-comment-return-inhibit-prefix nil
        vs-comment-return-cancel-after nil))
