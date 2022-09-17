;;; jcs-rust-mode.el --- Rust mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rust-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-rust-template "rust" "default.txt"
  "Header for Rust header file.")

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
                              'jcs-insert-rust-template))

(provide 'jcs-rust-mode)
;;; jcs-rust-mode.el ends here
