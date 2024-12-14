;;; misc/suggest/config.el  -*- lexical-binding: t; -*-

(use-package suggest
  :bind ( :map suggest-mode-map
          ("U"   . suggest-update)
          ("M-K" . suggest-update)))
