;;; ui/sideline/config.el  -*- lexical-binding: t; -*-

(use-package sideline
  :hook ((flycheck-mode . sideline-mode)
         (flymake-mode  . sideline-mode))
  :init
  (setq sideline-delay 0.2
        sideline-backends-left `((sideline-load-cost . up)
                                 (sideline-color     . up)
                                 (sideline-emoji     . up))
        sideline-backends-right `(((when (featurep 'lsp-mode)
                                     'sideline-lsp)
                                   . up)
                                  ((when (featurep 'eglot)
                                     'sideline-eglot)
                                   . up)
                                  ;; XXX: Too slow on windows!
                                  ((unless elenv-windows
                                     'sideline-blame)
                                   . up)
                                  ((when (featurep 'flycheck)
                                     'sideline-flycheck)
                                   . down)
                                  ((when (featurep 'flymake)
                                     'sideline-flymake)
                                   . down)
                                  ((when (featurep 'eros)
                                     'sideline-eros)
                                   . down)
                                  ((when (featurep 'cider)
                                     'sideline-cider)
                                   . down)
                                  ((when (featurep 'sly)
                                     'sideline-sly)
                                   . down)
                                  ((when (featurep 'geiser)
                                     'sideline-geiser)
                                   . down)
                                  ((when (featurep 'racket-mode)
                                     'sideline-racket)
                                   . down)
                                  ((when (featurep 'chatgpt)
                                     'chatgpt-sideline)
                                   . up))
        sideline-display-backend-name t
        sideline-display-backend-type 'inner
        sideline-truncate t))

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
