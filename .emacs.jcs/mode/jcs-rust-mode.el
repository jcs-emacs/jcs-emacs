;;; jcs-rust-mode.el --- Rust mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-rust-format ()
  "Format the given file as a Rust file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-rust-template)))

(require 'rust-mode)
(defun jcs-rust-mode-hook ()
  "Rust mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]rs" buffer-file-name) (jcs-rust-format))
          ))

  )
(add-hook 'rust-mode-hook 'jcs-rust-mode-hook)


(provide 'jcs-rust-mode)
;;; jcs-rust-mode.el ends here
