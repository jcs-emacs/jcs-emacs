;;; ui/vc-gutter/config.el  -*- lexical-binding: t; -*-

(setq vc-git-diff-switches '("--histogram"))  ;  A slightly faster algorithm for diffing

(use-package diff-hl
  :hook (find-file   . diff-hl-mode)
  :hook (vc-dir-mode . diff-hl-dir-mode)
  :hook (dired-mode  . diff-hl-dired-mode)
  :init
  (setq diff-hl-side 'right
        diff-hl-draw-borders nil
        ;; PERF: Slightly more conservative delay before updating the diff
        diff-hl-flydiff-delay 0.5  ; default: 0.3
        ;; PERF: don't block Emacs when updating vc gutter
        ;;
        ;; NOTE: Async feature is buggy for now.
        diff-hl-update-async nil
        ;; UX: get realtime feedback in diffs after staging/unstaging hunks
        diff-hl-show-staged-changes nil))

(defun jcs-diff-hl-update ()
  "Force update all `diff-hl' status."
  (dolist (buf (jcs-valid-buffer-list))
    (with-current-buffer buf
      (ignore-errors (diff-hl-update)))))
