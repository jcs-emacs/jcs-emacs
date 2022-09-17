;;; jcs-nix-mode.el --- Nix Expressions  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'nix-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-nix-template "nix" "default.txt"
  "Header for Nix header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nix-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]nix")
                              'jcs-insert-nix-template))

(provide 'jcs-nix-mode)
;;; jcs-nix-mode.el ends here
