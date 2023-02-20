;;; emacs/re-builder/config.el  -*- lexical-binding: t; -*-

(jcs-add-hook 'reb-mode-hook  ; Re-Builder
  (setq case-fold-search
        (string= "Case Sensitive"
                 (completing-read
                  "Enable case sensitive?" '("Case Sensitive"
                                             "Case Insensitive"))))

  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line)
      ((kbd "M-k")    . kill-buffer-and-window))))
