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

(jcs-add-hook 'fsharp-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]fs")
                              'jcs-insert-fsharp-template))

(provide 'jcs-fsharp-mode)
;;; jcs-fsharp-mode.el ends here
