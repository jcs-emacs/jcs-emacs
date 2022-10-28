;;; tools/eval/config.el  -*- lexical-binding: t; -*-

(leaf execrun
  :init
  (setq execrun-kill-buffer-function #'jcs-maybe-kill-this-buffer))

(leaf quickrun
  :init
  (setq quickrun-focus-p nil
        quickrun-truncate-lines nil))
