;;; emacs/undo/config.el  -*- lexical-binding: t; -*-

;; Increase undo history limits to reduce likelihood of data loss
(setq undo-limit 400000           ; 400kb (default is 160kb)
      undo-strong-limit 3000000   ; 3mb   (default is 240kb)
      undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

(leaf undo-tree-vf :hook (undo-tree-mode-hook . undo-tree-vf-mode))

(leaf undo-tree
  :init
  (setq undo-tree-auto-save-history nil)
  :defer-config
  (global-undo-tree-mode t))
