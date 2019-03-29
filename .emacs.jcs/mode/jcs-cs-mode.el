;;; jcs-cs-mode.el --- C# Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'csharp-mode)
(defun jcs-csharp-mode-hook ()

  (preproc-font-lock-mode t)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-csharp-format ()
    "Format the given file as a C# file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-cs-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]cs" buffer-file-name) (jcs-csharp-format))
          ))

  ;; jcs C# key binding
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

(add-to-list 'auto-mode-alist '("\\.cs?\\'" . csharp-mode))


(provide 'jcs-cs-mode)
;;; jcs-cs-mode.el ends here
