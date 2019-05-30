;;; jcs-erlang-mode.el --- Erlang Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-erlang-format ()
  "Format the given file as an Erlang file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-erlang-template)))

(require 'erlang)
(defun jcs-erlang-mode-hook ()
  "Erlang mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]erl" buffer-file-name) (jcs-erlang-format))
          ((string-match "[.]hrl" buffer-file-name) (jcs-erlang-format))
          ))

  )
(add-hook 'erlang-mode-hook 'jcs-erlang-mode-hook)


(provide 'jcs-erlang-mode)
;;; jcs-erlang-mode.el ends here
