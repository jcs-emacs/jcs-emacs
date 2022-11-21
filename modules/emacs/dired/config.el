;;; emacs/dired/config.el  -*- lexical-binding: t; -*-

(use-package dired
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))
