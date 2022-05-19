;;; jcs-nix-mode.el --- Nix Expressions  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'nix-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-nix-template ()
  "Header for Nix header file."
  (jcs--file-header--insert "nix" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nix-mode-hook
  (company-fuzzy-backend-add 'company-nixos-options)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]nix")
                              'jcs-insert-nix-template))

(provide 'jcs-nix-mode)
;;; jcs-nix-mode.el ends here
