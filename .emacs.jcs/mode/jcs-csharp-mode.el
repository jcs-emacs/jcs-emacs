;;; jcs-csharp-mode.el --- C# Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'csharp-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-csharp-mode-hook ()
  "Hook for C# mode."

  (setq-local docstr-show-type-name nil)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cs")
                              'jcs-csharp-ask-source
                              :interactive t)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  (jcs-bind-key [f8] #'jcs-find-corresponding-file)
  (jcs-bind-key [S-f8] #'jcs-find-corresponding-file-other-window)

  (jcs-bind-key (kbd "#") #'jcs-vs-sharp-key)

  (jcs-bind-key (kbd "M-q") #'jcs-other-window-prev))

(add-hook 'csharp-mode-hook 'jcs-csharp-mode-hook)

(provide 'jcs-csharp-mode)
;;; jcs-csharp-mode.el ends here
