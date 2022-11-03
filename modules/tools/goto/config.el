;;; tools/goto/config.el  -*- lexical-binding: t; -*-

(leaf goto-char-preview :hook (goto-char-preview-after-hook . repos-window-middle-after))
(leaf goto-line-preview :hook (goto-line-preview-after-hook . repos-window-middle-after))
