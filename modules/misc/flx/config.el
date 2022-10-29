;;; misc/flx/config.el  -*- lexical-binding: t; -*-

(leaf flx
  :defer-config
  (flx-rs-load-dyn)
  (advice-add 'flx-score :override #'flx-rs-score))
