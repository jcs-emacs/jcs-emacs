;;; tools/lsp/config.el  -*- lexical-binding: t; -*-

(use-package lsp-mode
  :init
  (setq lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        ;; Disable features that have great potential to be slow.
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-symbol-highlighting nil
        ;; Reduce unexpected modifications to code
        lsp-enable-on-type-formatting nil
        ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
        lsp-headerline-breadcrumb-enable nil
        ;; Make TCP connection is already slow; only try to connect once
        lsp-tcp-connection-timeout 0.01)

  (message-clean-mode-add-echo-commands
   '( lsp--message lsp--send-request-async lsp--apply-text-edits))

  (recentf-excl-add-commands '(lsp-find-references
                               lsp-find-declaration
                               lsp-find-implementation
                               lsp-find-type-definition))
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
  :bind ( :map lsp-ui-doc-frame-mode-map
          ([?q]  . jcs-poptip-unfocus)
          ("C-q" . jcs-poptip-unfocus))
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

(jcs-add-hook 'lsp-ui-doc-frame-mode-hook
  (scroll-bar-mode 1)
  (fill-page-mode 1))
