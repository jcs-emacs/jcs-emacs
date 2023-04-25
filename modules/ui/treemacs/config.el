;;; ui/treemacs/config.el  -*- lexical-binding: t; -*-

(use-package treemacs
  :init
  (setq treemacs-position 'right
        treemacs-missing-project-action 'remove
        treemacs-sorting 'alphabetic-asc
        treemacs-follow-after-init t
        treemacs-no-png-images t)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  (require 'treemacs-nerd-icons)

  (defvar jcs-treemacs-width-ratio 0.15
    "Ratio that respect to `frame-width' and `neo-window-width'.")

  (defun jcs-treemacs-toggle-refresh ()
    "Refresh `treemacs' by toggle twice."
    (save-selected-window (treemacs) (treemacs)))

  (jcs-add-hook 'window-size-change-functions
    (setq treemacs-width (round (* (frame-width) jcs-treemacs-width-ratio)))
    (when (treemacs-get-local-window) (jcs-treemacs-toggle-refresh)))

  (jcs-add-hook 'treemacs-mode-hook
    (setq buffer-wrap--relative-max-line 0)
    (buffer-wrap-mode 1)))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))
