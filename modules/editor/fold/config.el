;;; editor/fold/config.el  -*- lexical-binding: t; -*-

(leaf ts-fold
  :hook (tree-sitter-after-on-hook . ts-fold-indicators-mode)
  :init
  (setq ts-fold-indicators-fringe 'left-fringe
        ts-fold-indicators-face-function
        (lambda (pos &rest _)
          ;; Return the face of it's function.
          (line-reminder--get-face (line-number-at-pos pos t))))
  :defer-config
  (require 'line-reminder)
  (jcs-advice-add 'line-reminder-transfer-to-saved-lines :after
    ;; Refresh indicators for package `ts-fold'.
    (ts-fold-indicators-refresh)))
