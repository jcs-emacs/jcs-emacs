;;; jcs-csharp-mode.el --- C# Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'csharp-mode)
(defun jcs-csharp-mode-hook ()
  "CSharp mode hoo."
  (preproc-font-lock-mode t)
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-csharp-format ()
    "Format the given file as a C# file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-csharp-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]cs" buffer-file-name) (jcs-csharp-format))
          ))

  ;; Normal
  (define-key csharp-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key csharp-mode-map "\C-c\C-c" #'kill-ring-save)

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

  (define-key csharp-mode-map "\eq" #'jcs-other-window-prev)
  )
(add-hook 'csharp-mode-hook 'jcs-csharp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cs'?\\'" . csharp-mode))


(provide 'jcs-csharp-mode)
;;; jcs-csharp-mode.el ends here
