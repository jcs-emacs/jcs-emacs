;;; editor/regex/config.el  -*- lexical-binding: t; -*-

(use-package visual-regexp-steroids
  :init
  (setq vr/engine 'pcre2el))
