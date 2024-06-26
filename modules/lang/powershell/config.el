;;; lang/powershell/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-powershell-template "powershell" "default.txt"
  "Header for PowerShell header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'powershell-mode-hook
  (company-fuzzy-backend-add-before 'company-powershell 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]ps1")
                              'jcs-insert-powershell-template))
