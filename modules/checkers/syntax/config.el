;;; checkers/syntax/config.el  -*- lexical-binding: t; -*-

(use-package flycheck
  :bind ( :map flycheck-error-list-mode-map
          ("M-K" . flycheck-error-list-reset-filter))
  :init
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)

  (setq flycheck-indication-mode (if elenv-graphic-p 'right-fringe 'right-margin))

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s))
  (setq flycheck-display-errors-delay 0.25)
  :config
  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically))

(use-package flymake
  :init
  (setq flymake-fringe-indicator-position 'right-fringe))
