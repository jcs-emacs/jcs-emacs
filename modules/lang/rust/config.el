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
  ;; File Header
  (jcs-insert-header-if-valid '("[.]rs")
                              'jcs-insert-rust-template))

;;
;; (@* "Extensions" )
;;

(setq lsp-rust-analyzer-display-chaining-hints t
      lsp-rust-analyzer-display-parameter-hints t
      lsp-rust-analyzer-closing-brace-hints t)
