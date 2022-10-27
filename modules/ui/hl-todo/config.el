;;; ui/hl-todo/config.el  -*- lexical-binding: t; -*-

(leaf hl-todo
  :init
  (setq hl-todo-highlight-punctuation "")
  :defer-config
  (require 'asoc)
  (asoc-put! hl-todo-keyword-faces "TODO" "red" t)
  (asoc-put! hl-todo-keyword-faces "NOTE" "dark green" t)
  (asoc-put! hl-todo-keyword-faces "TEMP" "turquoise" t)
  (asoc-put! hl-todo-keyword-faces "FIXME" "red" t)
  (nconc hl-todo-keyword-faces
         '(("ATTENTION"   . "red")
           ("STUDY"       . "yellow")
           ("IMPORTANT"   . "yellow")
           ("CAUTION"     . "yellow")
           ("OPTIMIZE"    . "yellow")
           ("DESCRIPTION" . "dark green")
           ("TAG"         . "dark green")
           ("OPTION"      . "dark green")
           ("DEBUG"       . "turquoise")
           ("DEBUGGING"   . "turquoise")
           ("TEMPORARY"   . "turquoise")
           ("SOURCE"      . "PaleTurquoise2")
           ("URL"         . "PaleTurquoise2")
           ("IDEA"        . "green yellow")
           ("OBSOLETE"    . "DarkOrange3")
           ("DEPRECATED"  . "DarkOrange3")
           ("TOPIC"       . "slate blue")
           ("SEE"         . "slate blue")))
  (advice-add #'hl-todo--inside-comment-or-string-p :override #'jcs-inside-comment-or-string-p)
  (advice-add #'hl-todo-previous :after #'jcs--recenter--advice-after)
  (advice-add #'hl-todo-next :after #'jcs--recenter--advice-after))
