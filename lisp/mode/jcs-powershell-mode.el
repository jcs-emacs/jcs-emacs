;;; jcs-powershell-mode.el --- R programming language mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'powershell)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-powershell-template "powershell" "default.txt"
  "Header for PowerShell header file.")

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
