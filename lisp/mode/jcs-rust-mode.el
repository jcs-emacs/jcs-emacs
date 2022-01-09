;;; jcs-rust-mode.el --- Rust mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rust-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-rust-template ()
  "Header for Rust header file."
  (jcs--file-header--insert "rust" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'rust-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?! "w")

  (jcs-elec-pair-add '((?\' . ?\')))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]rs")
                              'jcs-insert-rust-template)

  (jcs-key-local
    `(((kbd "{") . jcs-vs-opening-curly-bracket-key))))

(provide 'jcs-rust-mode)
;;; jcs-rust-mode.el ends here
