;;; editor/docstring/config.el  -*- lexical-binding: t; -*-

(use-package highlight-doxygen-mode
  :hook (ts-docstr-mode . highlight-doxygen-mode))

(use-package ts-docstr
  :hook (tree-sitter-after-on . ts-docstr-mode)
  :init
  (setq ts-docstr-key-support nil
        ts-docstr-desc-summary "")
  :config
  (jcs-advice-add 'ts-docstr-key-enable :after
    (ts-docstr-key--advice-add "*" :around #'ts-docstr-key--doxygen-asterik)))
