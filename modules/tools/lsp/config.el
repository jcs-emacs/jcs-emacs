;;; tools/lsp/config.el  -*- lexical-binding: t; -*-

(use-package lsp-mode
  :init
  (setq lsp-auto-guess-root t
        lsp-keep-workspace-alive nil)
  :config
  ;; Let's not block the loading process, so lsp packages don't hamper with
  ;; each another.
  (jcs-advice-add 'lsp--require-packages :override
    (when (and lsp-auto-configure (not lsp--client-packages-required))
      (seq-do (lambda (package)
                ;; loading client is slow and `lsp' can be called repeatedly
                (unless (featurep package)
                  (ignore-errors (require package nil t))))
              lsp-client-packages)
      (setq lsp--client-packages-required t))))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-text-scale-level -1
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-delay 0.6
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-show-with-cursor nil
        lsp-eldoc-enable-hover nil
        lsp-ui-imenu-enable t
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face))
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t)
  :config
  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (lsp-ui-sideline-set-default-icon))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-emmet-completions t))
