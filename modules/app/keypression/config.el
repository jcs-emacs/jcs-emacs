;;; app/keypression/config.el  -*- lexical-binding: t; -*-

(use-package keypression
  :config
  (nconc keypression-ignore-mouse-events
         '( switch-frame menu-bar tool-bar tab-bar)))
