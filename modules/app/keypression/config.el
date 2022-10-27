;;; app/keypression/config.el  -*- lexical-binding: t; -*-

(leaf keypression
  :defer-config
  (nconc keypression-ignore-mouse-events
         '(switch-frame menu-bar tool-bar tab-bar)))
