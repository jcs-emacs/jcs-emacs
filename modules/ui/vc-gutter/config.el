;;; ui/vc-gutter/config.el  -*- lexical-binding: t; -*-

(setq vc-git-diff-switches '("--histogram"))  ;  A slightly faster algorithm for diffing

(use-package diff-hl
  :hook (find-file   . diff-hl-mode)
  :hook (vc-dir-mode . diff-hl-dir-mode)
  :hook (dired-mode  . diff-hl-dired-mode)
  :init
  (setq diff-hl-side 'right
        diff-hl-draw-borders nil
        diff-hl-flydiff-delay 0.5
        ;; UX: get realtime feedback in diffs after staging/unstaging hunks
        diff-hl-show-staged-changes nil))
