;;; editor/docstring/config.el  -*- lexical-binding: t; -*-

(leaf highlight-doxygen-mode
  :hook (ts-docstr-mode-hook . highlight-doxygen-mode))

(leaf ts-docstr
  :hook (tree-sitter-after-on-hook . ts-docstr-mode)
  :init
  (setq ts-docstr-key-support t
        ts-docstr-desc-summary ""))
