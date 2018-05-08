;; ========================================================================
;; $File: jcs-c++-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh C++ mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defun jcs-c++-mode-hook ()
  "C++ mode handling"

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (jcs-c++-header-format))
        ((string-match "[.]hpp" buffer-file-name) (jcs-c++-header-format))
        ((string-match "[.]h" buffer-file-name) (jcs-c++-header-format))

        ((string-match "[.]cin" buffer-file-name) (jcs-c++-source-format))
        ((string-match "[.]cpp" buffer-file-name) (jcs-c++-source-format))
        ((string-match "[.]c" buffer-file-name) (jcs-c-source-format))
        )

  ;; jcs C++ key binding
  (define-key c++-mode-map [f8] 'jcs-find-corresponding-file)
  (define-key c++-mode-map [S-f8] 'jcs-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the
  ;; corresponding file.
  (define-key c++-mode-map [f7] 'jcs-find-file-other-window)

  ;; Alternate bindings for F-keyless setups (ie MacOS X terminal)
  (define-key c++-mode-map "\ec" 'jcs-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'jcs-find-corresponding-file-other-window)

  (define-key c++-mode-map "\es" 'casey-save-buffer)

  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c++-mode-map [C-tab] 'indent-region)
  (define-key c++-mode-map "  " 'indent-region)
  ;;(define-key c++-mode-map [tab] '(lambda () (interactive) (insert "    ")))

  (define-key c++-mode-map "\ej" 'imenu)

  (define-key c++-mode-map "\e." 'c-fill-paragraph)

  (define-key c++-mode-map "\e/" 'c-mark-function)

  (define-key c++-mode-map "\eq" 'jcs-other-window-prev)
  (define-key c++-mode-map "\ea" 'yank)
  (define-key c++-mode-map "\ez" 'kill-region)

  ;; jcs-added
  (define-key c++-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key c++-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; Comment Block.
  (define-key c++-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key c++-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; Comement
  (define-key c++-mode-map (kbd "C-c s") 'jcs-toggle-c-comment-style)

  ;; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'casey-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(casey-devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4)))
  )
(add-hook 'c++-mode-hook 'jcs-c++-mode-hook)

(add-to-list 'auto-mode-alist '("\\.hin?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cin?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp?\\'" . c++-mode))


(defface jcs-c++-namespace-face
  '((t (:foreground "#38EFCA")))
  "Font face for namespace.")
(defvar jcs-c++-namespace-face 'jcs-c++-namespace-face)

;; Just in case, apply to `c-mode' too.
(defvar jcs-c++-font-lock-namespace '(cc-mode
                                      c-mode
                                      c++-mode)
  "Font lock for namespace.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("::\\([a-zA-Z0-9_]*\\)" 1 'jcs-c++-namespace-face t)
           )'end))
      jcs-c++-font-lock-namespace)
