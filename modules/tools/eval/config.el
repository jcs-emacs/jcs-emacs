;;; tools/eval/config.el  -*- lexical-binding: t; -*-

(use-package easky
  :init
  (setq easky-display-function #'lv-message
        easky-focus-p t
        easky-move-point-for-output t))

(use-package execrun
  :init
  (setq execrun-kill-buffer-function #'jcs-maybe-kill-this-buffer))

(use-package quickrun
  :init
  (setq quickrun-focus-p nil
        quickrun-truncate-lines nil))
