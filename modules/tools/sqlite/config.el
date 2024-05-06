;;; tools/sqlite/config.el  -*- lexical-binding: t; -*-

(use-package sqlite-mode
  :bind ( :map sqlite-mode-map
          ("M-K" . sqlite-mode-list-tables)
          ("TAB" . sqlite-mode-list-data)))
