;;; tools/eval/config.el  -*- lexical-binding: t; -*-

(use-package easky
  :init
  (setq easky-display-function #'lv-message
        easky-focus-p nil
        easky-move-point-for-output t))

(use-package execrun
  :init
  (setq execrun-kill-buffer-function #'jcs-maybe-kill-this-buffer)

  (message-clean-mode-add-echo-commands '( execrun-compile)))

(use-package quickrun
  :hook (quickrun--mode . (lambda (&rest _)
                            (setq buffer-face-mode-face '(:height 120))
                            (buffer-face-mode 1)))
  :init
  (setq quickrun-focus-p nil
        quickrun-truncate-lines nil
        quickrun-timeout-seconds -1))
