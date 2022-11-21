;;; tools/goto/config.el  -*- lexical-binding: t; -*-

(use-package goto-char-preview
  :hook (goto-char-preview-after . repos-window-middle-after))
(use-package goto-line-preview
  :hook (goto-line-preview-after . repos-window-middle-after))
