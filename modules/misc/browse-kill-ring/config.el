;;; misc/browse-kill-ring/config.el  -*- lexical-binding: t; -*-

(use-package browse-kill-ring
  :bind ( :map browse-kill-ring-mode-map
          ("<escape>" . kill-current-buffer)))
