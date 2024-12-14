;;; misc/with-editor/config.el  -*- lexical-binding: t; -*-

(use-package with-editor
  :bind ( :map with-editor-mode-map
          ("C-s"      . with-editor-finish)
          ("C-g"      . with-editor-cancel)
          ("<escape>" . with-editor-cancel)))
