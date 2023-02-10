;;; ui/sideline/config.el  -*- lexical-binding: t; -*-

(use-package sideline
  :hook ((flycheck-mode . sideline-mode)
         (flymake-mode  . sideline-mode))
  :init
  (setq sideline-delay 0.2
        sideline-backends-left '((sideline-color . up))
        sideline-backends-right '((sideline-lsp      . up)
                                  (sideline-flycheck . down)
                                  (sideline-flymake  . down))
        sideline-display-backend-name t
        sideline-display-backend-type 'inner))

(use-package sideline-flycheck :hook (flycheck-mode . sideline-flycheck-setup))

(use-package sideline-lsp
  :init
  (setq sideline-lsp-code-actions-prefix ""))
