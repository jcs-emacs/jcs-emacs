;;; editor/multiple-cursors/config.el  -*- lexical-binding: t; -*-

(use-package iedit
  :init
  (setq iedit-toggle-key-default nil
        iedit-auto-save-occurrence-in-kill-ring nil))

(use-package vsc-multiple-cursors
  :hook (multiple-cursors-mode . vsc-multiple-cursors-mode)
  :bind ( :map mc/keymap
          ("<escape>" . mc/keyboard-quit)
          ("<return>")
          ("C-v"      . vsc-edit-yank)
          ("C-:")
          ("C-'"))
  :init
  (setq vsc-multiple-cursors-cancel-commands
        '( block-travel-down block-travel-up
           jcs-isearch-backward-symbol-at-point
           isearch-forward-symbol-at-point
           jcs-isearch-repeat-backward
           jcs-isearch-repeat-forward
           jcs-isearch-project-backward-symbol-at-point
           isearch-project-forward-symbol-at-point
           jcs-isearch-project-repeat-backward
           jcs-isearch-project-repeat-forward)))
