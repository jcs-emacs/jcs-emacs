;;; emacs/undo/config.el  -*- lexical-binding: t; -*-

(leaf undo-tree
  :init
  (setq undo-tree-auto-save-history nil)
  :defer-config
  (global-undo-tree-mode t))

(leaf undo-tree-vf :hook (undo-tree-mode-hook . undo-tree-vf-mode))
