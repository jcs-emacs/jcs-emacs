;;; jcs-powershell-mode.el --- R programming language mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'powershell)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-powershell-template ()
  "Header for PowerShell header file."
  (jcs--file-header--insert "powershell" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'powershell-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (company-fuzzy-backend-add 'company-powershell)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]ps1")
                              'jcs-insert-powershell-template))

(provide 'jcs-powershell-mode)
;;; jcs-powershell-mode.el ends here
