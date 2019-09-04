;;; jcs-csharp-mode.el --- C# Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'csharp-mode)


(defun jcs-csharp-mode-hook ()
  "CSharp mode hook."
  (preproc-font-lock-mode t)
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cs")
                              'jcs-csharp-ask-source
                              t)

  ;; Normal
  (define-key csharp-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key csharp-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key csharp-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  (define-key csharp-mode-map [f8] #'jcs-find-corresponding-file)
  (define-key csharp-mode-map [S-f8] #'jcs-find-corresponding-file-other-window)

  (define-key csharp-mode-map (kbd "<up>") #'jcs-csharp-smart-indent-up)
  (define-key csharp-mode-map (kbd "<down>") #'jcs-csharp-smart-indent-down)

  ;; comment block
  (define-key csharp-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key csharp-mode-map (kbd "*") #'jcs-c-comment-pair)

  (define-key csharp-mode-map (kbd "/") #'jcs-vs-csharp-maybe-insert-codedoc)

  (define-key csharp-mode-map (kbd "#") #'jcs-vs-sharp-key)

  (define-key csharp-mode-map (kbd "M-q") #'jcs-other-window-prev)
  )
(add-hook 'csharp-mode-hook 'jcs-csharp-mode-hook)


(provide 'jcs-csharp-mode)
;;; jcs-csharp-mode.el ends here
