;;; ui/breadcrumb/config.el  -*- lexical-binding: t; -*-

(use-package breadcrumb
  :config
  ;; Influenced by https://github.com/joaotavora/breadcrumb/commit/04c50e32e8f32afd68242e4dc28da02e8a45e237
  (advice-remove 'mode--line-format-right-align 'breadcrumb-opinionated-mlf))
