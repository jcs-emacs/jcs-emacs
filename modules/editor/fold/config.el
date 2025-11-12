;;; editor/fold/config.el  -*- lexical-binding: t; -*-

(use-package ts-fold
  :hook (tree-sitter-after-on . ts-fold-mode)
  :hook (tree-sitter-after-on . ts-fold-line-comment-mode))

(use-package foldvis
  :init
  (setq foldvis-fringe 'left-fringe
        foldvis-face-function (lambda (pos &rest _)
                                ;; Return the face of it's function.
                                (line-reminder--get-face (line-number-at-pos pos t))))
  :config
  (elenv-uappend foldvis-commands
    '( vs-edit-fold-open
       vs-edit-fold-open-all
       vs-edit-fold-close
       vs-edit-fold-close-all))

  (require 'line-reminder)
  (setq line-reminder-add-line-function
        (lambda (&rest _)
          (null (foldvis--overlays-in 'foldvis-window (selected-window)
                                      (line-beginning-position) (line-end-position))))))

(use-package savefold
  :init
  (jcs-with-eval-after-load 'outline  (savefold-outline-mode 1))
  (jcs-with-eval-after-load 'hideshow (savefold-hideshow-mode 1))
  (jcs-with-eval-after-load 'hideif   (savefold-hide-ifdef-mode 1))
  (jcs-with-eval-after-load 'org      (savefold-org-mode 1))
  (jcs-with-eval-after-load 'ts-fold  (savefold-ts-fold-mode 1)))

(use-package fold-this
  :init
  (setq fold-this-overlay-text (truncate-string-ellipsis)))
