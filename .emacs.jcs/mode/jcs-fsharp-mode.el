;;; jcs-fsharp-mode.el --- C# Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'fsharp-mode)

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
