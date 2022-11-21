;;; misc/flx/config.el  -*- lexical-binding: t; -*-

(use-package flx
  :config
  (flx-rs-load-dyn)
  (advice-add 'flx-score :override #'flx-rs-score))
