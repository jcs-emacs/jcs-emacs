;; This is the start of jcs-web-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-web-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-web-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-web-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Web Development mode.
;;
;; include HTML,CSS,PHP,JavaScript,JSON.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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


;; ======================
;; web-mode.el
;; homepage - http://web-mode.org/

;; list of  extensions that will take effect of this mode
(require 'web-mode)
(defun jcs-web-mode-hook ()
  "Hooks for Web mode."

  ;; enable impatient mode for real time editing.
  (impatient-mode t)

  (setq web-mode-markup-indent-offset 2)

  ;; 1 (default) for double quotes, 2 for single quotes
  (setq web-mode-auto-quote-style 1)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; enable truncate lines.
  (toggle-truncate-lines)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; align truncate lines use.
  (visual-line-mode t)

  (require 'ac-php)
  (setq ac-sources '(ac-source-php))


  (defun jcs-html-format ()
    "Format the give file. - JenChieh HTML file"
    (interactive)

    (if (is-current-file-empty-p)
        (progn
          ;; macro
          (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

          ;; insert tag header
          (jcs-tag-file-info)

          ;; insert HTML common format
          (insert "<!DOCTYPE html>\n")
          (insert "<html>\n")
          (insert "<head>\n")
          (insert "<title>")
          (push-mark)
          (insert BaseFileName)
          (pop-mark)
          (insert "</title>\n")
          (insert "<meta charset=\"UTF-8\">\n\n")
          (insert "</head>\n")
          (insert "<body>\n\n\n")
          (insert "</body>\n")
          (insert "</html>\n")

          ;; format the document once
          (jcs-format-document)
          ))
    )

  (defun jcs-php-format ()
    "Format the give file. - JenChieh PHP file"
    (interactive)

    (if (is-current-file-empty-p)
        (progn
          ;; insert PHP common format
          (insert "<?php\n")

          (jcs-global-file-info)
          (insert "\n\n")

          (insert "?>\n")
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]html" buffer-file-name) (jcs-html-format))
        ((string-match "[.]php" buffer-file-name) (jcs-php-format))
        )

  ;; jcs web key binding
  (define-key web-mode-map (kbd "C-d") 'jcs-web-kill-whole-line)
  (define-key web-mode-map "\C-c\C-c" 'jcs-web-kill-ring-save)
  (define-key web-mode-map "\C-v" 'jcs-web-yank)

  (define-key web-mode-map "\C-k\C-f" 'jcs-web-indent-region)
  (define-key web-mode-map "\C-k\C-d" 'jcs-web-format-document)
  (define-key web-mode-map (kbd "C-S-f") 'jcs-web-format-region-or-document)

  ;; Save
  (define-key web-mode-map "\C-s" 'jcs-web-save-buffer)

  ;; comment block
  (define-key web-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key web-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; Shortcuts
  (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)

  ;; PHP
  (define-key web-mode-map [C-backspace] 'jcs-web-backward-delete-word)
  (define-key web-mode-map (kbd "C-M-<backspace>") 'jcs-web-backward-delete-word-capital)
  (define-key emmet-mode-keymap (kbd "C-M-<left>") 'jcs-backward-capital-char)
  (define-key emmet-mode-keymap (kbd "C-M-<right>") 'jcs-forward-capital-char)
  )
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook  'jcs-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))       ;; Add .php to the list

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
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-block-padding 0)

;; Comments
;;(setq web-mode-comment-style 2)

;; Syntax Highlighting
(set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")
(set-face-attribute 'web-mode-block-comment-face nil :foreground "olive drab")
(set-face-attribute 'web-mode-comment-face nil :foreground "olive drab")

;; Snippets
(setq web-mode-extra-snippets
      '(("erb" . (("toto" . ("<% toto | %>\n\n<% end %>"))))
        ("php" . (("dowhile" . ("<?php do { ?>\n\n<?php } while (|); ?>"))
                  ("debug" . ("<?php error_log(__LINE__); ?>"))))
        ))

;; Auto-pairs
(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))
        ))

;; Enable / disable features
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)

;; Keywords / Constants
;;(setq web-mode-extra-constants '(("php" . ("CONS1" "CONS2"))))

;; Highlight current HTML element
(setq web-mode-enable-current-element-highlight t)

;; You can also highlight the current column with
(setq web-mode-enable-current-column-highlight t)

;; Context-aware auto-completion
(setq web-mode-ac-sources-alist
      '(("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("css" . (ac-source-css-property))
        ("php" . (ac-source-php ac-source-words-in-buffer ac-source-abbrev))
        ))

;;=====================
;; ac-html
(defun setup-ac-for-html ()
  ;; Require ac-haml since we are setup haml auto completion
  (require 'ac-haml)
  ;; Require default data provider if you want to use
  (require 'ac-html-default-data-provider)
  ;; Enable data providers,
  ;; currently only default data provider available
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  ;; Let ac-haml do some setup
  (ac-haml-setup)
  ;; Set your ac-source
  (setq ac-sources '(ac-source-haml-tag
                     ac-source-haml-attr
                     ac-source-haml-attrv))
  ;; Enable auto complete mode
  (auto-complete-mode))
(add-hook 'haml-mode-hook 'setup-ac-for-html)

;; ac-php
(add-hook 'php-mode-hook '(lambda ()
                            (auto-complete-mode t)
                            (require 'ac-php)
                            (setq ac-sources '(ac-source-php))
                            (yas-global-mode 1)

                            ;; highlight URL and clickable.
                            (goto-address-mode 1)

                            (define-key php-mode-map (kbd "C-]") 'ac-php-find-symbol-at-point)   ; goto define
                            (define-key php-mode-map (kbd "C-t") 'ac-php-location-stack-back)    ; go back

                            ;; jcs PHP key binding
                            (define-key php-mode-map (kbd "C-d") 'jcs-kill-whole-line)
                            (define-key php-mode-map "\C-c\C-c" 'kill-ring-save)
                            ))


;;============================
;; CSS editing

(require 'emmet-mode)
(require 'rainbow-mode)

;; css indent spaces.
(setq css-indent-offset 2)

;; Source: -> CSS Mode: https://www.emacswiki.org/emacs/css-mode.el
;;         -> Xah CSS Mode: http://ergoemacs.org/emacs/xah-css-mode.html
(load-file "~/.emacs.d/elisp/css-mode.el")
(load-file "~/.emacs.d/elisp/css-sort.el")

(require 'css-sort)
(require 'css-mode)
(defun jcs-css-mode-hook ()

  ;; enable impatient mode for real time editing.
  (impatient-mode t)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  (defun jcs-css-format()
    "Format the give file. - JenChieh CSS file"
    (interactive)

    (if (is-current-file-empty-p)
        (progn
          (insert "@charset \"UTF-8\";\n")

          (jcs-global-file-info)
          (insert "\n\n")
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]css" buffer-file-name) (jcs-css-format))
        )

  ;; Set Faces.
  (setq-local font-lock-comment-face '(jdee-font-lock-javadoc-face))
  (jcs-init-css-faces)

  ;; jcs web key binding
  (define-key css-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key css-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key skewer-css-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; Save
  (define-key css-mode-map "\C-s" 'jcs-css-save-buffer)

  ;; comment block
  (define-key css-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key css-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; sort attribute in order => `css-sort' package.
  (define-key css-mode-map "\C-ks" 'jcs-css-sort-attributes)
  (define-key css-mode-map "\C-kd" 'jcs-css-sort-attributes-document)
  )
(add-hook 'css-mode-hook  'jcs-css-mode-hook)
(add-hook 'css-mode-hook 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.css'?" . css-mode))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-web-mode.el file
