;;; lang/nix/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-nix-template "nix" "default.txt"
  "Header for Nix header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nix-mode-hook
  (company-fuzzy-backend-add 'company-nixos-options)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]nix")
                              'jcs-insert-nix-template))
