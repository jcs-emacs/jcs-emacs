;;; completion/corfu/config.el  -*- lexical-binding: t; -*-

(use-package corfu
  :init
  (setq corfu-auto t
        corfu-quit-no-match t
        corfu-quit-at-boundary 'separator
        corfu-scroll-margin 0
        corfu-count 10
        corfu-auto-delay 0.07
        corfu-preview-current nil
        corfu-cycle t
        corfu-auto-prefix 0
        corfu-popupinfo-mode t
        corfu-popupinfo-delay 0.3
        corfu-bar-width 1
        corfu-popupinfo-delay))

(setq corfu-sort-override-function #'vertico-flx-sort-default)

