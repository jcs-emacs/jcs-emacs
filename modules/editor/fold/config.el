;;; editor/fold/config.el  -*- lexical-binding: t; -*-

(use-package ts-fold
  :hook (tree-sitter-after-on . ts-fold-line-comment-mode)
  :hook (tree-sitter-after-on . ts-fold-indicators-mode)
  :hook (ts-fold-on-fold      . sideline-render-this)
  :init
  (setq ts-fold-indicators-fringe 'left-fringe
        ts-fold-indicators-face-function
        (lambda (pos &rest _)
          ;; Return the face of it's function.
          (line-reminder--get-face (line-number-at-pos pos t))))
  :config
  (require 'line-reminder)
  (setq line-reminder-add-line-function
        (lambda (&rest _)
          (null (ts-fold--overlays-in 'ts-fold-indicators-window (selected-window)
                                      (line-beginning-position) (line-end-position)))))
  (jcs-advice-add 'line-reminder-transfer-to-saved-lines :after
    ;; Refresh indicators for package `ts-fold'.
    (ts-fold-indicators-refresh)))
