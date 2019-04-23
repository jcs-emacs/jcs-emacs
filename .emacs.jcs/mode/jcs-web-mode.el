;;; jcs-web-mode.el --- Web Development mode. -*- lexical-binding: t -*-
;;; Commentary: Including HTML, CSS, PHP, JavaScript, JSON.
;;; Code:


;;============================
;; Truncate Lines
;;URL(jenchieh): https://emacs.stackexchange.com/questions/14589/correct-indentation-for-wrapped-lines

(require 'adaptive-wrap)

(with-eval-after-load 'adaptive-wrap
  (setq-default adaptive-wrap-extra-indent 0))

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


;; ======================
;; web-mode.el
;; homepage - http://web-mode.org/

;; list of  extensions that will take effect of this mode
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


  (defun jcs-html-format ()
    "Format the give file as a HTML file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-html-template)))

  (defun jcs-php-format ()
    "Format the give file as a PHP file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-php-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]html" buffer-file-name) (jcs-html-format))
        ((string-match "[.]asp" buffer-file-name) (jcs-html-format))
        ((string-match "[.]as[cp]x" buffer-file-name) (jcs-html-format))
        ((string-match "[.]php" buffer-file-name) (jcs-php-format))
        )

  ;; Set Faces.
  (jcs-init-web-faces)

  ;; Normal
  (define-key web-mode-map (kbd "C-d") #'jcs-web-kill-whole-line)
  (define-key web-mode-map "\C-c\C-c" #'jcs-web-kill-ring-save)
  (define-key emmet-mode-keymap "\C-c\C-c" #'jcs-web-kill-ring-save)
  (define-key web-mode-map "\C-v" #'jcs-web-yank)

  (define-key web-mode-map "\C-k\C-f" #'jcs-web-indent-region)
  (define-key web-mode-map "\C-k\C-d" #'jcs-web-format-document)
  (define-key web-mode-map (kbd "C-S-f") #'jcs-web-format-region-or-document)

  (define-key web-mode-map [f8] #'jcs-find-corresponding-file)
  (define-key web-mode-map [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; Edit
  ;;
  ;; TODO(jenchieh): `web-mode' usually have the issue with indentation,
  ;; see if these keys still needed.
  ;;(define-key web-mode-map (kbd "<up>") #'jcs-web-smart-indent-up)
  ;;(define-key web-mode-map (kbd "<down>") #'jcs-web-smart-indent-down)
  (define-key web-mode-map (kbd "RET") #'jcs-web-return-key)

  (define-key web-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key web-mode-map (kbd "{") #'jcs-web-vs-front-curly-bracket-key)
  (define-key web-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; Save
  ;;
  ;; TODO(jenchieh): `web-mode' usually have the issue with indentation,
  ;; see if these keys still needed.
  ;;(define-key web-mode-map "\C-s" #'jcs-web-save-buffer)

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


;; Associate an engine
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )

;; Associate a content type
(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

(setq web-mode-content-types-alist
      '(("json" . "/some/path/.*\\.api\\'")
        ("xml"  . "/other/path/.*\\.api\\'")
        ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))


;;;=======================

;;; Indentation
;; NOTE: HTML element offset indentation
(setq web-mode-markup-indent-offset 2)
;; NOTE: CSS offset indentation
(setq web-mode-css-indent-offset 2)
;; NOTE: Script/code offset indentation (for JavaScript,
;;                                           Java,
;;                                           PHP,
;;                                           Ruby,
;;                                           Go,
;;                                           VBScript,
;;                                           Python, etc.)
(setq web-mode-code-indent-offset 2)

(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

;; Left padding
(setq web-mode-style-padding 2)   ;; For `<style>' tag
(setq web-mode-script-padding 2)  ;; For `<script>' tag
(setq web-mode-block-padding 0)   ;; For `php', `ruby', `java', `python', `asp', etc.

;;; Offsetless Elements
;; NOTE(jenchieh): Do not make these lists to one list.
;; They are totally different list.
;; NOTE(jenchieh): This variable is from `web-mode' itself.
(setq web-mode-offsetless-elements '("html"))

;; NOTE(jenchieh): Do not make these lists to one list.
;; They are totally different list.
(defvar jcs-web-mode-offsetless-elements-toggle '("html")
  "List of HTML elements you want to be toggable to the
`wen-mode-offsetless-elements' list in Web mode.")

;;; Comments
;;(setq web-mode-comment-style 2)

;; Syntax Highlighting
(set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")
(set-face-attribute 'web-mode-block-comment-face nil :foreground "olive drab")
(set-face-attribute 'web-mode-comment-face nil :foreground "olive drab")

;;; Snippets
(setq web-mode-extra-snippets
      '(("erb" . (("toto" . ("<% toto | %>\n\n<% end %>"))))
        ("php" . (("dowhile" . ("<?php do { ?>\n\n<?php } while (|); ?>"))
                  ("debug" . ("<?php error_log(__LINE__); ?>"))))
        ))

;;; Auto-pairs
(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))
        ))

;;; Enable / disable features
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)

;;; Keywords / Constants
;;(setq web-mode-extra-constants '(("php" . ("CONS1" "CONS2"))))

;; Highlight current HTML element
(setq web-mode-enable-current-element-highlight t)

;; You can also highlight the current column with
(setq web-mode-enable-current-column-highlight t)


(provide 'jcs-web-mode)
;;; jcs-web-mode.el ends here
