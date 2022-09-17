;;; jcs-fsharp-mode.el --- C# Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'fsharp-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-fsharp-template "fsharp" "default.txt"
  "Header for F# header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'fsharp-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]fs")
                              'jcs-insert-fsharp-template))

(provide 'jcs-fsharp-mode)
;;; jcs-fsharp-mode.el ends here
