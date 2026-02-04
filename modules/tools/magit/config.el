;;; tools/magit/config.el  -*- lexical-binding: t; -*-

(require 'diff-hl)
(require 'magit-lfs)

(use-package magit
  :hook (magit-post-refresh . jcs-diff-hl-update)
  :init
  (setq magit-auto-revert-mode nil  ; we do this ourselves further down
        magit-diff-refine-hunk t    ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil
        ;; Display file icons
        magit-format-file-function #'magit-format-file-nerd-icons)

  (msg-clean-add-echo-commands
   '( magit-process-sentinel magit-commit-diff magit-run-git-async
      git-commit-save-message)))

(use-package magit-todos
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil)
        magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?"))

(use-package eldoc-diffstat
  :init
  ;; XXX: Not sure why the `:hook' keyword doesn't work.k
  (jcs-add-hook 'magit-status-mode-hook
    (eldoc-diffstat-setup))

  (eldoc-add-command
   'magit-next-line 'magit-previous-line
   'magit-section-forward 'magit-section-backward
   'magit-section-forward-sibling 'magit-section-backward-sibling))
