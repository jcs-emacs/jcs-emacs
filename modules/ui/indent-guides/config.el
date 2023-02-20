;;; ui/indent-guides/config.el  -*- lexical-binding: t; -*-

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-suppress-auto-error t))
