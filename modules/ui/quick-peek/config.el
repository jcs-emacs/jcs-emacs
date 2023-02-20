;;; ui/quick-peek/config.el  -*- lexical-binding: t; -*-

(use-package scrollable-quick-peek
  :bind ( :map scrollable-quick-peek-keymap
          ("<down>")
          ("<up>")
          ("S-<down>" . scrollable-quick-peek-scroll-down)
          ("S-<up>"   . scrollable-quick-peek-scroll-up)))
