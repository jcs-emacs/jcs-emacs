;;; jcs-elixir-mode.el --- Elixir Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-elixir-format ()
  "Format the given file as an Elixir file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-elixir-template)))

(require 'elixir-mode)
(defun jcs-elixir-mode-hook ()
  "Elixir mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]ex" buffer-file-name) (jcs-elixir-format))
          ((string-match "[.]exs" buffer-file-name) (jcs-elixir-format))
          ))

  )
(add-hook 'elixir-mode-hook 'jcs-elixir-mode-hook)


(provide 'jcs-elixir-mode)
;;; jcs-elixir-mode.el ends here
