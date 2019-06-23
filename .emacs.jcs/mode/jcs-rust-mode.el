;;; jcs-rust-mode.el --- Rust mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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
          ((string-match "[.]rs" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-rust-template))
          ))

  )
(add-hook 'rust-mode-hook 'jcs-rust-mode-hook)


(provide 'jcs-rust-mode)
;;; jcs-rust-mode.el ends here
