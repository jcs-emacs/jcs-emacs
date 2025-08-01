;;; emacs/dired/config.el  -*- lexical-binding: t; -*-

(require 'autorevert)

;; XXX: Make list directories first!
(setq ls-lisp-dirs-first t
      ls-lisp-use-insert-directory-program nil)

(use-package dired
  :hook (dired-mode . buffer-wrap-mode)
  :bind ( :map dired-mode-map
          ("M-<up>"    . dired-up-directory)
          ("M-<left>"  . dired-up-directory)
          ("M-<right>" . dired-find-file)
          ("C-<"       . dired-gitignore-mode)
          ("C->"       . dired-omit-mode)
          ("<f2>"      . dired-efap))
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
        image-dired-thumb-size 150
        ;; Kill the buffer when switch to new buffer.
        dired-kill-when-opening-new-dired-buffer t)
  :config
  (dired-gitignore-global-mode 1))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-git-info
  :hook (dired-after-readin . dired-git-info-auto-enable)
  :init
  (setq dgi-auto-hide-details-p nil))

(use-package dired-efap
  :config
  ;; Doesn't compatible to `dired-git-info'.
  (jcs-add-hook 'dired-efap-mode-hooks (dired-git-info-mode -1)))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
