;;; tools/magit/config.el  -*- lexical-binding: t; -*-

(use-package magit
  :init
  (setq magit-auto-revert-mode nil  ; we do this ourselves further down
        magit-diff-refine-hunk t    ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)

  (message-clean-mode-add-echo-commands
   '( magit-process-sentinel magit-commit-diff magit-run-git-async
      git-commit-save-message)))

(use-package magit-todos
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil)
        magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?"))
