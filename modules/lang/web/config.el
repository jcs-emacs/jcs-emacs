;;; lang/web/config.el  -*- lexical-binding: t; -*-

(require 'sgml-mode)
(require 'web-mode)

(require 'impatient-mode)
(require 'auto-rename-tag)
(require 'emmet-mode)
(require 'htmltagwrap)

;;
;; (@* "Impatient Mode" )
;;

(defun jcs-impatient-mode (args)
  "Default `impatient-mode' function by ARGS."
  (if (= args 1)
      (progn
        (unless (process-status "httpd") (httpd-start))
        (impatient-mode args)
        (imp-set-user-filter nil)
        (imp-visit-buffer))
    (httpd-stop)
    (impatient-mode args)))

(defun jcs-impatient-by-mode (args)
  "Enable/Disable `impatient-mode' by ARGS"
  (require 'impatient-mode)
  (cl-case major-mode
    (`markdown-mode (impatient-showdown-mode args))
    (t (jcs-impatient-mode args))))

(defun jcs-impatient-start ()
  "Start real time editing with default port."
  (interactive)
  (jcs-impatient-by-mode 1)
  (message "[INFO] Start real time editing with port: %d" httpd-port httpd-port))

(defun jcs-impatient-stop ()
  "Shutdown real time editing with default port."
  (interactive)
  (jcs-impatient-by-mode -1)
  (message "[INFO] Shutdown real time editing with port: %d" httpd-port))

;;
;; (@* "Other" )
;;

(defun jcs-emmet-expand-line ()
  "Wrapper of `emmet-expand-line' function."
  (interactive)
  (if (jcs-current-point-face 'link)
      (call-interactively #'goto-address-at-point)
    (unless (call-interactively #'emmet-expand-line)
      (jcs-ctrl-return-key))))

;;
;; (@* "Deletion" )
;;

(defun jcs-web-backward-delete-word ()
  "Web backward delete the word, fit PHP variable naming."
  (interactive)
  (backward-delete-char 1)
  (when (and (not (jcs-current-whitespace-or-tab-p))
             (not (jcs-current-char-equal-p "$"))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word)))

(defun jcs-web-backward-delete-word-capital ()
  "Web backward delete word capital, fit PHP variable naming."
  (interactive)
  (backward-delete-char 1)
  (when (and (not (jcs-current-whitespace-or-tab-p))
             (not (jcs-current-char-equal-p "$"))
             (not (jcs-current-char-uppercasep))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word-capital))
  (when (and (jcs-current-char-uppercasep)
             (not (jcs-current-char-equal-p "$")))
    (backward-delete-char 1)))

;;
;; (@* "Indentation" )
;;

(defun jcs-web-vs-opening-curly-bracket-key ()
  "Web mode front curly bracket key."
  (interactive)
  (if (jcs-current-point-face '(web-mode-script-face
                                web-mode-block-face
                                web-mode-style-face))
      (call-interactively #'vs-edit-opening-curly-bracket-key)
    (insert "{}")
    (backward-char 1)))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-html-template "html" "default.txt"
  "Template for HTML.")

(file-header-defins jcs-insert-php-template "php" "default.txt"
  "Template for PHP.")

;;
;; (@* "Hook" )
;;

(leaf web-mode
  :init
  (setq web-mode-markup-indent-offset 2  ; html
        web-mode-css-indent-offset 2     ; css
        web-mode-code-indent-offset 2    ; script
        web-mode-style-padding 2   ; For `<style>' tag
        web-mode-script-padding 2  ; For `<script>' tag
        web-mode-block-padding 0   ; For `php', `ruby', `java', `python', `asp', etc.
        web-mode-offsetless-elements '("html")))

(add-hook 'web-mode-hook 'emmet-mode)

(jcs-add-hook 'web-mode-hook
  (setq truncate-lines t)
  (jcs-elec-pair-add '((?\' . ?\') (?\" . ?\")))

  (auto-rename-tag-mode 1)
  (visual-line-mode t)
  (impatient-mode t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]htm" "[.]html"
                                "[.]asp"
                                "[.]as[cp]x")
                              'jcs-insert-html-template)
  (jcs-insert-header-if-valid '("[.]php")
                              'jcs-insert-php-template)

  (jcs-key-local
    `(((kbd "{")   . jcs-web-vs-opening-curly-bracket-key)

      ;; Shortcuts
      ((kbd "C-n") . web-mode-tag-match)

      ;; PHP
      ([C-backspace]         . jcs-web-backward-delete-word)
      ((kbd "M-<backspace>") . jcs-web-backward-delete-word-capital)))

  ;; Emmet
  (jcs-key emmet-mode-keymap
    `(((kbd "C-<return>") . jcs-emmet-expand-line))))

(jcs-add-hook 'html-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))

;;
;; (@* "Extensions" )
;;

(leaf adaptive-wrap
  :hook (visual-line-mode-hook . adaptive-wrap-prefix-mode))

(leaf auto-rename-tag
  :init
  (setq auto-rename-tag-disabled-commands '(query-replace)
        auto-rename-tag-disabled-minor-modes '(iedit-mode
                                               multiple-cursors-mode)))
