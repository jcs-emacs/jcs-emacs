;;; jcs-rust-mode.el --- Rust mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'rust-mode)
(defun jcs-rust-mode-hook ()
  "Rust mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-rust-format ()
    "Format the given file as a Rust file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-rust-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]rs" buffer-file-name) (jcs-rust-format))
          ))

  )
(add-hook 'rust-mode-hook 'jcs-rust-mode-hook)

(add-to-list 'auto-mode-alist '("\\.rs'?\\'" . rust-mode))


(provide 'jcs-rust-mode)
;;; jcs-rust-mode.el ends here
