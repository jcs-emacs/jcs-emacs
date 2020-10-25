;;; jcs-powershell-mode.el --- R programming language mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'powershell)

;;
;; (@* "Hook" )
;;

(defun jcs-powershell-mode-hook ()
  "PowerShell mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]ps1")
                              'jcs-insert-powershell-template))

(add-hook 'powershell-mode-hook 'jcs-powershell-mode-hook)

(provide 'jcs-powershell-mode)
;;; jcs-powershell-mode.el ends here
