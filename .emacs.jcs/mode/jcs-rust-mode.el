;;; jcs-rust-mode.el --- Rust mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'rust-mode)


(defun jcs-rust-mode-hook ()
  "Rust mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]rs")
                              'jcs-insert-rust-template)

  )
(add-hook 'rust-mode-hook 'jcs-rust-mode-hook)


(provide 'jcs-rust-mode)
;;; jcs-rust-mode.el ends here
