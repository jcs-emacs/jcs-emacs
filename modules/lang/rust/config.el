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
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?! "w")

  (jcs-elec-pair-add '((?\' . ?\')))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]rs")
                              'jcs-insert-rust-template))
