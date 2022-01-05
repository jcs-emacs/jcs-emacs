;;; jcs-markdown-mode.el --- Markdown mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'markdown-mode)
(require 'web-mode)

(setq markdown-fontify-code-blocks-natively t)

(nconc markdown-code-lang-modes'(("cs" . csharp-mode)
                                 ("el" . emacs-lisp-mode)
                                 ("cl" . lisp-mode)))

(defun jcs-markdown-return-key ()
  "Return key for Markdown mode."
  (interactive)
  (let (did-ret-key close-tag-found)
    (when (and (jcs-first-forward-char-in-line-p "<")
               (jcs-first-backward-char-in-line-p ">"))
      ;; Check closing tag.
      (save-excursion
        (jcs-move-to-forward-a-char "<")
        (forward-char 1)
        (setq close-tag-found (jcs-current-char-equal-p "/")))

      (when close-tag-found
        (newline-and-indent)
        (newline-and-indent)
        (jcs-smart-indent-up)
        (setq did-ret-key t)))

    (unless did-ret-key
      (newline)
      (indent-for-tab-command))))

;;
;; (@* "Faces" )
;;

(defun jcs-init-markdown-faces ()
  "Customize face for `markdown-mode'."
  ;; General
  (set-face-attribute 'markdown-markup-face nil
                      :background (face-background 'default)
                      :foreground "#7EA728")
  ;; Code Block
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil
                      :foreground (face-foreground 'default)
                      :background "#2B2B2B"
                      :extend t)
  ;; List
  (set-face-attribute 'markdown-list-face nil
                      :foreground "gold3")
  ;; Table
  (set-face-attribute 'markdown-table-face nil
                      :foreground "#87CEFA"
                      :background (face-background 'default))
  ;; Header
  (set-face-attribute 'markdown-header-face nil
                      :foreground "#B5CCEB"
                      :background (face-background 'default))
  (set-face-attribute 'markdown-header-delimiter-face nil
                      :foreground "#B5CCEB"
                      :background (face-background 'default)))

(jcs-init-markdown-faces)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-markdown-template ()
  "Header for Markdown header file."
  (jcs--file-header--insert "markdown" "default.txt"))

;;
;; (@* "Hook" )
;;

(add-hook 'markdown-mode-hook 'emmet-mode)

(defun jcs-markdown-mode-hook ()
  "Markdown mode hook."
  )

(jcs-add-hook 'markdown-mode-hook
  (emojify-mode 1)

  (jcs-safe-er/expand-list '(web-mode-mark-and-expand) t)

  (jcs-elec-pair-add '((?\` . ?\`)))

  (jcs-company-safe-add-backend 'company-emojify)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]md" "[.]markdown")
                              'jcs-insert-markdown-template)

  ;; Normal
  (jcs-bind-key (kbd "<backspace>") #'jcs-real-backspace)
  (jcs-bind-key (kbd "RET") #'jcs-markdown-return-key)

  (jcs-bind-key [S-tab] #'markdown-cycle)

  ;; Eemmet
  (define-key emmet-mode-keymap (kbd "C-<return>") #'jcs-emmet-expand-line))


(provide 'jcs-markdown-mode)
;;; jcs-markdown-mode.el ends here
