;;; editor/fold/config.el  -*- lexical-binding: t; -*-

(use-package ts-fold
  :hook (tree-sitter-after-on . ts-fold-mode)
  :hook (tree-sitter-after-on . ts-fold-line-comment-mode)
  :hook (tree-sitter-after-on . ts-fold-indicators-mode)
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
                                      (line-beginning-position) (line-end-position))))))

(use-package savefold
  :init
  (jcs-with-eval-after-load 'outline  (savefold-outline-mode 1))
  (jcs-with-eval-after-load 'hideshow (savefold-hideshow-mode 1))
  (jcs-with-eval-after-load 'hideif   (savefold-hide-ifdef-mode 1))
  (jcs-with-eval-after-load 'org      (savefold-org-mode 1))
  (jcs-with-eval-after-load 'ts-fold  (savefold-ts-fold-mode 1)))
