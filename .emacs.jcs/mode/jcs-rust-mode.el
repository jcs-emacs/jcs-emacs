;;; jcs-rust-mode.el --- Rust mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rust-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-rust-mode-hook ()
  "Rust mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]rs")
                              'jcs-insert-rust-template)

  ;; Comment Block
  (define-key rust-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key rust-mode-map (kbd "*") #'jcs-c-comment-pair))

(add-hook 'rust-mode-hook 'jcs-rust-mode-hook)

(provide 'jcs-rust-mode)
;;; jcs-rust-mode.el ends here
