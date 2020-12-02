;;; jcs-nix-mode.el --- Nix Expressions  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'nix-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-nix-mode-hook ()
  "Hook for `nix-mode'."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]nix")
                              'jcs-insert-nix-template))

(add-hook 'nix-mode-hook 'jcs-nix-mode-hook)

(provide 'jcs-nix-mode)
;;; jcs-nix-mode.el ends here
