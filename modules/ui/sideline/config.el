;;; ui/sideline/config.el  -*- lexical-binding: t; -*-

(use-package sideline
  :hook ((flycheck-mode . sideline-mode)
         (flymake-mode  . sideline-mode))
  :init
  (setq sideline-delay 0.2
        sideline-backends-left `((sideline-load-cost . up)
                                 (sideline-color     . up))
        sideline-backends-right `((sideline-lsp      . up)
                                  (sideline-eglot    . up)
                                  (sideline-flycheck . down)
                                  (sideline-flymake  . down)
                                  (chatgpt-sideline  . up))
        sideline-display-backend-name t
        sideline-display-backend-type 'inner))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup)
  :init
  (setq sideline-flycheck-display-mode 'line))

(use-package sideline-flymake
  :init
  (setq sideline-flymake-display-mode 'line))

(use-package sideline-lsp
  :init
  (setq sideline-lsp-code-actions-prefix ""))

(use-package sideline-eglot
  :init
  (setq sideline-eglot-code-actions-prefix ""))

(use-package sideline-blame
  :init
  (setq sideline-blame-commit-format "• %s"))
