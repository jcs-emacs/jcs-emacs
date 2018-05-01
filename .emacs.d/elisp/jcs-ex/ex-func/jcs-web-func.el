;; This is the start of jcs-web-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-web-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Wed Jun 21 13:51:49 EST 2017>
;; Time-stamp: <2017-07-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-web-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-web-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the HTML related file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;---------------------------------------------
;; Deletion
;;---------------------------------------------

;;;###autoload
(defun jcs-web-kill-whole-line ()
  "Kill whole line in web-mode."
  (interactive)
  (jcs-kill-whole-line)

  ;; NOTE(jenchieh): Unknown reason that web-mode will
  ;; get disable...
  (web-mode)

  ;; NOTE(jenchieh): Get back highlighting.
  ;;(font-lock-flush)
  ;;(font-lock-fontify-buffer)
  )

;;;###autoload
(defun jcs-web-kill-ring-save ()
  "Kill ring save in web-mode."
  (interactive)
  (kill-ring-save (region-beginning) (region-end))

  ;; NOTE(jenchieh): Unknown reason that web-mode will
  ;; get disable...
  (web-mode)

  ;; NOTE(jenchieh): Get back highlighting.
  ;;(font-lock-flush)
  ;;(font-lock-fontify-buffer)
  )

;;;###autoload
(defun jcs-web-yank ()
  "Yank in web-mode. No idea why, yank function just need to get
wrap by another function..."
  (interactive)

  ;; if region, delete the region first.
  (when (use-region-p)
    ;; NOTE(jayces): `kill-region' will copy the word.
    ;; Use `delete-region' instead, this will not copy
    ;; the word.
    (delete-region (region-beginning) (region-end)))

  ;; then paste it.
  (yank)

  ;; NOTE(jenchieh): Unknown reason that web-mode will
  ;; get disable...
  (web-mode)

  ;; NOTE(jenchieh): Get back highlighting.
  ;;(font-lock-flush)
  ;;(font-lock-fontify-buffer)
  )

;;;###autoload
(defun jcs-web-backward-delete-word ()
  "Web backward delete the word, fit PHP variable naming."
  (interactive)

  (backward-delete-char 1)

  (when (and (not (current-whitespacep))
             (not (current-char-equal-p "$"))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word)))

;;;###autoload
(defun jcs-web-backward-delete-word-capital ()
  "Web backward delete word capital, fit PHP variable naming."
  (interactive)

  (backward-delete-char 1)

  (when (and (not (current-whitespacep))
             (not (current-char-equal-p "$"))
             (not (jcs-current-char-uppercasep))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word-capital))

  (when (and (jcs-current-char-uppercasep)
             (not (current-char-equal-p "$")))
    (backward-delete-char 1)))

;;========================================
;;      JCS Format File
;;----------------------------------

;;;###autoload
(defun jcs-web-indent-region ()
  "Indent region for `web-mode'."
  (interactive)
  (save-excursion
    (setq endLineNum (string-to-number (format-mode-line "%l")))

    (goto-char (region-beginning))
    (setq startLineNum (string-to-number (format-mode-line "%l")))

    (exchange-point-and-mark)

    (goto-char (region-end))
    (setq endLineNum2 (string-to-number (format-mode-line "%l")))

    (goto-char (region-beginning))
    (setq startLineNum2 (string-to-number (format-mode-line "%l")))

    (deactivate-mark)

    (goto-line startLineNum)
    (previous-line 1)

    (while (and (<= (string-to-number (format-mode-line "%l")) endLineNum))
      (jcs-web-smart-indent-down)
      (end-of-line))

    (goto-line endLineNum2)
    (next-line 1)

    (while (and (>= (string-to-number (format-mode-line "%l")) startLineNum2))
      (jcs-web-smart-indent-up)
      (end-of-line))))

;;;###autoload
(defun jcs-web-format-document ()
  "Indent the whoe document line by line instead of indent it \
once to the whole document.  For `web-mode'."
  (interactive)

  (save-excursion
    (save-window-excursion
      (let ((endPos nil))
        (goto-char (point-max))
        (setq endPos (point))

        (goto-char (point-min))

        (while (and (< (point) endPos))
          (jcs-web-smart-indent-down)
          (end-of-line))))))

;;;###autoload
(defun jcs-web-format-region-or-document ()
  "Format the document if there are no region apply.
For `web-mode' we specificlly indent through the file
line by line instead of indent the whole file at once."
  (interactive)

  (if (use-region-p)
      (progn
        (call-interactively 'jcs-web-indent-region))
    (progn
      (call-interactively 'jcs-web-format-document))))

;;---------------------------------------------
;; Indentation
;;---------------------------------------------

;;;###autoload
(defun jcs-web-smart-indent-up ()
  "Smart indent up for `web-mdoe'."
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name))
      (progn
        (previous-line 1)
        (jcs-delete-space-infront-of-line)
        (indent-for-tab-command))
    (previous-line 1)))

;;;###autoload
(defun jcs-web-smart-indent-down ()
  "Smart indent down for `web-mdoe'."
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name))
      (progn
        (next-line 1)
        (jcs-delete-space-infront-of-line)
        (indent-for-tab-command))
    (next-line 1)))

;;;###autoload
(defun jcs-web-return ()
  "Return key for `web-mode'."
  (interactive)

  ;; Call defulat function first.
  (jcs-smart-context-line-break)

  (save-excursion
    ;; Fix curly bracket not indent correctly.
    (jcs-web-smart-indent-down)))

;;---------------------------------------------
;; Save
;;---------------------------------------------

;;;###autoload
(defun jcs-web-save-buffer ()
  "Save buffer in `web-mode'."
  (interactive)
  (jcs-web-format-document)
  (visual-line-mode t)
  (jcs-untabify-save-buffer))

;;;###autoload
(defun jcs-css-save-buffer ()
  "Save buffer in `css-mode'."
  (interactive)
  (com-css-sort-attributes-document)
  (jcs-untabify-save-buffer))

;;---------------------------------------------
;; Impatient Mode
;;---------------------------------------------

;;;###autoload
(defun jcs-httpd-start ()
  "Active real time editing with default port. (`impatient-mode')"
  (interactive)
  ;; NOTE(jayces): port can be change at `jcs-web-mode.el' file.
  (message "Active real time editing with port: %d" httpd-port)
  (call-interactively 'httpd-start))

;;;###autoload
(defun jcs-httpd-stop ()
  "Close real time editing with default port. (`impatient-mode')"
  (interactive)
  ;; NOTE(jayces): port can be change at `jcs-web-mode.el' file.
  (message "Close real time editing with port: %d" httpd-port)
  (call-interactively 'httpd-stop))


;;---------------------------------------------
;; CSS
;;---------------------------------------------

(defun jcs-init-css-faces ()
  "CSS Faces Highlighting."

  ;; ==================
  ;;    Type Face
  ;; -----------------
  (defface jcs-css-type-face
    '((t (:foreground "#38EFCA")))
    "Highlight CSS value.")
  (defvar jcs-css-type-face 'jcs-css-type-face)

  (defvar jcs-css-type-modes '(css-mode)
    "CSS mode we want to add it to highlight the type face.")

  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("^[ \t]*\\([a-z0-9_-]*\\)[ \t]*:" 1 'jcs-css-type-face t)
             )'end))
        jcs-css-type-modes)

  ;; ==================
  ;;    Value Face
  ;; -----------------
  (defface jcs-css-value-face
    '((t (:foreground "#D2D2D2")))
    "Highlight CSS value.")
  (defvar jcs-css-value-face 'jcs-css-value-face)

  (defvar jcs-css-value-modes '(css-mode)
    "CSS mode we want to add it to highlight the value face.")

  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("[ \t]*:[ \t]*\\(.*\\)[ \t]*;" 1 'jcs-css-value-face t)
             )'end))
        jcs-css-value-modes)

  ;; Other faces.
  (setq-local font-lock-function-name-face '(:foreground "#17A0FB"))
  (setq-local font-lock-variable-name-face '(:foreground "#38EFCA")))

(defun jcs-init-web-faces ()
  "Web mode Faces Highlihgting."

  (defface jcs-web-mode-html-attr-value-face
    '((t (:foreground "olive drab")))
    "Highlight HTML value.")
  (defvar jcs-web-mode-html-attr-value-face 'jcs-web-mode-html-attr-value-face)

  (face-remap-add-relative 'web-mode-block-string-face '(jcs-font-lock-string-face))
  (face-remap-add-relative 'web-mode-html-attr-value-face '(jcs-web-mode-html-attr-value-face)))

;;-----------------------------------------------------------
;;-----------------------------------------------------------

;;;###autoload
(defun jcs-toggle-web-mode-offsetless-elements ()
  "Toggle between indent with html tag or not to."
  (interactive)
  (if (get 'jcs-toggle-web-mode-offsetless-elements 'state)
      (progn
        (dolist (tmp-element jcs-web-mode-offsetless-elements-toggle)
          (push tmp-element web-mode-offsetless-elements))
        (put 'jcs-toggle-web-mode-offsetless-elements 'state nil))
    (progn
      (dolist (tmp-element jcs-web-mode-offsetless-elements-toggle)
        (setq web-mode-offsetless-elements (remove tmp-element web-mode-offsetless-elements)))
      (put 'jcs-toggle-web-mode-offsetless-elements 'state t))))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-web-func.el file
