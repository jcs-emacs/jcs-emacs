;;; emacs/hexl/config.el  -*- lexical-binding: t; -*-

(use-package hexl-mode
  :bind ( :map hexl-mode-map
          ("M-k"   . jcs-maybe-kill-current-buffer)
          ("M-K"   . jcs-reopen-this-buffer)
          ("C-M-k" . kill-current-buffer)

          ("C-k C-p" . package-list-packages)
          ("C-S-x"   . package-list-packages)))
