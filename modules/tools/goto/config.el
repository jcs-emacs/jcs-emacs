;;; tools/goto/config.el  -*- lexical-binding: t; -*-

(leaf goto-char-preview :hook (goto-char-preview-after-hook . jcs--recenter--advice-after))
(leaf goto-line-preview :hook (goto-line-preview-after-hook . jcs--recenter--advice-after))
