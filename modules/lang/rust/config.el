;;; lang/rust/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-rust-template "rust" "default.txt"
  "Header for Rust header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'rust-mode-hook
  (modify-syntax-entry ?! "w")

  (jcs-elec-pair-add '((?\' . ?\')))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]rs")
                              'jcs-insert-rust-template))

;;
;; (@* "Extensions" )
;;

(setq lsp-rust-analyzer-display-chaining-hints t
      lsp-rust-analyzer-closing-brace-hints t)
