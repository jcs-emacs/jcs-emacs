;;; tools/eval/config.el  -*- lexical-binding: t; -*-

(use-package execrun
  :init
  (setq execrun-kill-buffer-function #'jcs-maybe-kill-this-buffer))

(use-package quickrun
  :init
  (setq quickrun-focus-p nil
        quickrun-truncate-lines nil))
