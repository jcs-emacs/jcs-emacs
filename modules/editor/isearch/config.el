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
                                       "res/"))
  :config
  (jcs-add-hook 'isearch-mode-hook
    ;; Paste the current symbol when `isearch' enabled.
    (cond ((use-region-p)
           (progn
             (deactivate-mark)
             (ignore-errors
               (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))))
          ((memq this-command '(jcs-isearch-project-backward-symbol-at-point))
           (when (char-or-string-p isearch-project--thing-at-point)
             (backward-word 1)
             (isearch-project--isearch-yank-string isearch-project--thing-at-point)
             (isearch-repeat-backward))))))
