;;; editor/docstring/config.el  -*- lexical-binding: t; -*-

(use-package highlight-doxygen
  :hook (ts-docstr-mode . highlight-doxygen-mode))

(use-package ts-docstr
  :hook (tree-sitter-after-on . ts-docstr-mode)
  :init
  (setq ts-docstr-key-support nil
        ts-docstr-desc-summary ""))
