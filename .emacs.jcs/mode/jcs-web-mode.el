;;; jcs-web-mode.el --- Web Development mode. -*- lexical-binding: t -*-
;;; Commentary: Including HTML, CSS, PHP, JavaScript, JSON.
;;; Code:


;;============================
;; Truncate Lines
;;URL: https://emacs.stackexchange.com/questions/14589/correct-indentation-for-wrapped-lines

(require 'adaptive-wrap)

(add-hook 'visual-line-mode-hook
          (lambda ()
            (adaptive-wrap-prefix-mode +1)
            (diminish 'visual-line-mode)))


;;============================
;; Real time editing mark down (impatient-mode)

;; Note for "Impatient Mode" (real time editing)
;; Step 1: M-x httpd-start        (Open the port default: 8080)
;; Step 2: M-x impatient-mode     (Enabled Impatient Mode)

(require 'impatient-mode)


;;============================
;;; Web Dev useful packages.
(require 'auto-rename-tag)
(require 'emmet-mode)
(require 'htmltagwrap)


(require 'web-mode)
(defun jcs-web-mode-hook ()
  "Hooks for Web mode."

  (setq web-mode-markup-indent-offset 2)

  ;; 1 (default) for double quotes, 2 for single quotes
  (setq web-mode-auto-quote-style 1)

  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)
  (visual-line-mode t)
  (impatient-mode t)

  ;; Enable truncates lines as default in `web-mode'.
  (jcs-enable-truncate-lines)

  (jcs-web-enable-auto-truncate-lines)

  ;; Docstring Faces
  (face-remap-add-relative 'jcs-oop-tag-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'jcs-oop-type-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'jcs-oop-value-face '(:inherit web-mode-block-face))
  (face-remap-add-relative 'web-mode-block-string-face '(jcs-font-lock-string-face))
  (face-remap-add-relative 'web-mode-html-attr-value-face '(jcs-web-mode-html-attr-value-face))

  (jcs-make-electric-pair-pairs-local '((?\' . ?\')))
  (jcs-make-electric-pair-pairs-local '((?\" . ?\")))

  (cond ((file-exists-p buffer-file-name) t)
        ((or (string-match "[.]html" buffer-file-name)
             (string-match "[.]asp" buffer-file-name)
             (string-match "[.]as[cp]x" buffer-file-name))
         (jcs-insert-header-if-empty 'jcs-insert-html-template))
        ((string-match "[.]php" buffer-file-name)
         (jcs-insert-header-if-empty 'jcs-insert-php-template))
        )

  ;; Normal
  (define-key web-mode-map (kbd "C-d") #'jcs-web-kill-whole-line)
  (define-key web-mode-map (kbd "C-c C-c") #'jcs-web-kill-ring-save)
  (define-key emmet-mode-keymap (kbd "C-c C-c") #'jcs-web-kill-ring-save)
  (define-key web-mode-map (kbd "C-v") #'jcs-web-yank)

  (define-key web-mode-map [f8] #'jcs-find-corresponding-file)
  (define-key web-mode-map [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; Edit
  (define-key web-mode-map (kbd "RET") #'jcs-web-return-key)

  (define-key web-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key web-mode-map (kbd "{") #'jcs-web-vs-front-curly-bracket-key)
  (define-key web-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key web-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; Shortcuts
  (define-key web-mode-map (kbd "C-n") #'web-mode-tag-match)

  ;; Emmet
  (define-key emmet-mode-keymap (kbd "C-<return>") #'jcs-emmet-expand-line)

  ;; PHP
  (define-key web-mode-map [C-backspace] #'jcs-web-backward-delete-word)
  (define-key web-mode-map (kbd "M-<backspace>") #'jcs-web-backward-delete-word-capital)
  (define-key emmet-mode-keymap (kbd "M-<left>") #'jcs-backward-capital-char)
  (define-key emmet-mode-keymap (kbd "M-<right>") #'jcs-forward-capital-char)
  )
(add-hook 'web-mode-hook  'jcs-web-mode-hook)
(add-hook 'web-mode-hook 'emmet-mode)


(provide 'jcs-web-mode)
;;; jcs-web-mode.el ends here
