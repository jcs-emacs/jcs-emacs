;;; ui/indent-guides/config.el  -*- lexical-binding: t; -*-

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-suppress-auto-error t)
  (setq highlight-indent-guides-auto-character-face-perc 150
        highlight-indent-guides-auto-top-character-face-perc 250
        highlight-indent-guides-auto-stack-character-face-perc 200))
