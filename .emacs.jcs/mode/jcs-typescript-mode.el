;; ========================================================================
;; $File: jcs-typescript-mode.el $
;; $Date: 2018-10-11 16:03:58 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(require 'typescript-mode)
(defun jcs-typescript-mode-hook ()
  "TypeScript mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-typescript-format ()
    "Format the given file as a class. - JenChieh TypScript file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-typescript-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]ts" buffer-file-name) (jcs-typescript-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs TypeScript key binding
  (define-key typescript-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key typescript-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key typescript-mode-map (kbd "DEL") #'jcs-delete-backward-char)
  (define-key typescript-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key typescript-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key typescript-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key typescript-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'typescript-mode-hook 'jcs-typescript-mode-hook)

(add-to-list 'auto-mode-alist '("\\.ts'?\\'" . typescript-mode))
