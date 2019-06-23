;;; jcs-erlang-mode.el --- Erlang Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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
          ((or (string-match "[.]erl" buffer-file-name)
               (string-match "[.]hrl" buffer-file-name))
           (jcs-insert-header-if-empty 'jcs-insert-erlang-template))
          ))

  )
(add-hook 'erlang-mode-hook 'jcs-erlang-mode-hook)


(provide 'jcs-erlang-mode)
;;; jcs-erlang-mode.el ends here
