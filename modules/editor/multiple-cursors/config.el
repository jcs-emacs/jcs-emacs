;;; editor/multiple-cursors/config.el  -*- lexical-binding: t; -*-

(leaf iedit
  :init
  (setq iedit-toggle-key-default nil
        iedit-auto-save-occurrence-in-kill-ring nil))

(leaf vsc-multiple-cursors
  :hook (multiple-cursors-mode-hook . vsc-multiple-cursors-mode)
  :init
  (setq vsc-multiple-cursors-cancel-commands
        '( block-travel-down block-travel-up
           jcs-isearch-backward-symbol-at-point
           isearch-forward-symbol-at-point
           jcs-isearch-repeat-backward
           jcs-isearch-repeat-forward
           jcs-isearch-project-backward-symbol-at-point
           isearch-project-forward-symbol-at-point
           jcs-isearch-project-repeat-backward
           jcs-isearch-project-repeat-forward)))
