;;; jcs-fsharp-mode.el --- C# Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'fsharp-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-fsharp-template ()
  "Header for F# header file."
  (jcs--file-header--insert "fsharp" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-fsharp-mode-hook ()
  "Hook for F# mode."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]fs")
                              'jcs-insert-fsharp-template))

(add-hook 'fsharp-mode-hook 'jcs-fsharp-mode-hook)

(provide 'jcs-fsharp-mode)
;;; jcs-fsharp-mode.el ends here
