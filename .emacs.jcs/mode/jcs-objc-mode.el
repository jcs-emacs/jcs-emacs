;; ========================================================================
;; $File: jcs-objc-mode.el $
;; $Date: 2018-11-20 23:54:16 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(defun jcs-objc-mode-hook ()
  "Objective-C mode hook."

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (jcs-objc-header-format))
        ((string-match "[.]hpp" buffer-file-name) (jcs-objc-header-format))
        ((string-match "[.]h" buffer-file-name) (jcs-objc-header-format))

        ((string-match "[.]cin" buffer-file-name) (jcs-objc-source-format))
        ((string-match "[.]cpp" buffer-file-name) (jcs-objc-source-format))
        ((string-match "[.]c" buffer-file-name) (jcs-objc-source-format))
        ((string-match "[.]m" buffer-file-name) (jcs-objc-source-format))
        )

  ;; jcs Objective-C key binding
  (define-key objc-mode-map [f8] #'jcs-find-corresponding-file)
  (define-key objc-mode-map [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the
  ;; corresponding file.
  (define-key objc-mode-map [f7] #'jcs-find-file-other-window)

  ;; Alternate bindings for F-keyless setups (ie MacOS X terminal)
  (define-key objc-mode-map "\ec" #'jcs-find-corresponding-file)
  (define-key objc-mode-map "\eC" #'jcs-find-corresponding-file-other-window)

  (define-key objc-mode-map "\ej" #'imenu)

  (define-key objc-mode-map "\e." #'c-fill-paragraph)

  (define-key objc-mode-map "\e/" #'c-mark-function)

  (define-key objc-mode-map "\eq" #'jcs-other-window-prev)
  (define-key objc-mode-map "\ea" #'yank)
  (define-key objc-mode-map "\ez" #'kill-region)

  ;; jcs-added
  (define-key objc-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key objc-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key objc-mode-map (kbd "DEL") #'jcs-delete-backward-char)
  (define-key objc-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key objc-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; Comment Block.
  (define-key objc-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key objc-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; Comement
  (define-key objc-mode-map (kbd "C-c s") #'jcs-toggle-c-comment-style)

  ;; Undo/Redo
  (define-key objc-mode-map "\C-z" #'jcs-undo)
  (define-key objc-mode-map "\C-y" #'jcs-redo)

  ;; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'casey-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(casey-devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4)))
  )
(add-hook 'objc-mode-hook 'jcs-objc-mode-hook)

(add-to-list 'auto-mode-alist '("\\.m'?\\'" . objc-mode))
