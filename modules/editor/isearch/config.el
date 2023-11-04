;;; editor/isearch/config.el  -*- lexical-binding: t; -*-

(use-package isearch
  :hook
  ((isearch-mode     . better-scroll-revert)
   (isearch-mode-end . better-scroll-setup))
  :bind ( :map isearch-mode-map
          ("C-s")
          ("C-r")
          ("C-," . jcs-isearch-repeat-backward)
          ("C-." . jcs-isearch-repeat-forward)
          ("C-<" . jcs-isearch-project-repeat-backward)
          ("C->" . jcs-isearch-project-repeat-forward)
          ("C-x" . kill-region)
          ("C-c" . kill-ring-save)
          ("C-v" . isearch-yank-pop))
  :init
  (setq isearch-lazy-count t
        lazy-count-prefix-format "[%s:%s] "))

(use-package isearch-project
  :init
  (setq isearch-project-ignore-paths '("bin/"
                                       "build/"
                                       "build.min/"
                                       "res/")))
