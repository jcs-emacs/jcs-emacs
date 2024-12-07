;;; ui/sideline/config.el  -*- lexical-binding: t; -*-

(use-package sideline
  :hook ((flycheck-mode   . sideline-mode)
         (flymake-mode    . sideline-mode)
         ;; Modes
         (emacs-lisp-mode . (lambda ()
                              (when (featurep 'eros) (sideline-mode 1))))
         (lisp-mode       . (lambda ()
                              (when (featurep 'sly) (sideline-mode 1))))
         (clojure-mode    . (lambda ()
                              (when (featurep 'cider) (sideline-mode 1))))
         (scheme-mode     . (lambda ()
                              (when (featurep 'geiser) (sideline-mode 1))))
         (racket-mode     . (lambda ()
                              (when (featurep 'racket-mode) (sideline-mode 1)))))
  :init
  (setq sideline-delay 0.2
        sideline-backends-left `((sideline-load-cost . up)
                                 (sideline-color     . up))
        sideline-backends-right `((sideline-lsp      . up)
                                  (sideline-eglot    . up)
                                  (sideline-flycheck . down)
                                  (sideline-flymake  . down)
                                  (sideline-eros     . down)
                                  (sideline-cider    . down)
                                  (sideline-sly      . down)
                                  (sideline-geiser   . down)
                                  (sideline-racket   . down)
                                  (chatgpt-sideline  . up))
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
