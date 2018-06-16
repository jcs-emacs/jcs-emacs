;; ========================================================================
;; $File: jcs-web-func.el $
;; $Date: 2017-07-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the HTML related file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;----------------------------------------------
;; Web Comment Face
;;----------------------------------------------

(defface jcs-web-mode-block-comment-face
  '((t (:inherit 'jcs-font-lock-comment-face :background "#000000")))
  "Web mode block comment face with dark background."
  :group 'jcs-web-faces)
(defvar jcs-web-mode-block-comment-face 'jcs-web-mode-block-comment-face)


(defvar jcs-web-type-comment-missing-modes '(web-mode)
  "Modes that does not apply comment in ASP.NET (Razor v3) Syntax.")


(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(;; For nomral HTML comment.
           ("\\(<!--[a-zA-Z0-9 \n\t-.<>?,*'`@\"=_(){}:;&^%$#!~]*-->\\)" 1 'jcs-font-lock-comment-face t)
           ;; For multi-lines comment.
           ;; TODO(jenchieh): Only inside the curly bracket.
           ("\\(/\\*[a-zA-Z0-9 \n\t-.<>?,*'`@\"=_(){}:;&^%$#!~]*\\*/\\)" 1 'jcs-web-mode-block-comment-face t)
           ;; For one line comment.
           ;; TODO(jenchieh): Only inside the curly bracket.
           ("\\(/\\*[a-zA-Z0-9 \t-.<>?,*'`/@\"=_(){}:;&^%$#!~]*\\*/\\)" 1 'jcs-web-mode-block-comment-face t)
           )'end))
      jcs-web-type-comment-missing-modes)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-web-mode ()
  "Rewrap of switching mode to `web-mode'."
  (web-mode)
  (jcs-web-keep-auto-truncate-lines))

;;-----------------------------------------------------------
;; Truncate lines depends on the Face.
;;-----------------------------------------------------------

(defvar jcs-web-auto-truncate-lines nil
  "Toggle between using automatically truncate lines or not.")

(defun jcs-web-truncate-lines-by-face ()
  "Enable/Disable the truncate lines mode depends on the face \
cursor currently on."
  (interactive)
  (save-excursion
    (when (not (jcs-current-line-empty-p))
      ;; NOTE(jenchieh): When cursor is at the end of
      ;; line, the face will always be `default' face.
      ;; Which mean this will always be mis-detected,
      ;; we fixed this by just backward a character.
      ;;
      ;; Forward a char if is at the end of line.
      (when (or
             (jcs-is-default-face-p)
             ;; If current charact is either space or tab.
             (jcs-current-char-string-match-p "[ \t]"))
        (jcs-goto-first-char-in-line)
        (forward-char 1))

      (if (jcs-is-default-face-p)
          (jcs-disable-truncate-lines)
        (jcs-enable-truncate-lines)))))

;;;###autoload
(defun jcs-web-enable-auto-trancate-lines ()
  "Enable auto trancate lines effect."
  (interactive)
  (setq jcs-web-auto-truncate-lines t)
  (message "Enable auto truncate lines."))

;;;###autoload
(defun jcs-web-disable-auto-trancate-lines ()
  "Disable auto trancate lines effect."
  (interactive)
  (setq jcs-web-auto-truncate-lines nil)
  (message "Disable auto truncate lines."))

;;;###autoload
(defun jcs-web-toggle-auto-truncate-lines ()
  "Toggle `jcs-web-auto-truncate-lines' variables."
  (interactive)
  (if jcs-web-auto-truncate-lines
      (jcs-web-disable-auto-trancate-lines)
    (jcs-web-enable-auto-trancate-lines)))

;;;###autoload
(defun jcs-web-keep-auto-truncate-lines ()
  "Keep the same trigger for auto truncate mode.
Is the opposite of `jcs-web-toggle-auto-truncate-lines'."
  (interactive)
  (if jcs-web-auto-truncate-lines
      (jcs-web-enable-auto-trancate-lines)
    (jcs-web-disable-auto-trancate-lines)))

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
  ;;(jcs-web-mode)

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
  ;;(jcs-web-mode)

  ;; NOTE(jenchieh): Get back highlighting.
  ;;(font-lock-flush)
  ;;(font-lock-fontify-buffer)
  )

;;;###autoload
(defun jcs-web-yank ()
  "Yank in web-mode.
No idea why, yank function just need to get wrap by
another function..."
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
  ;;(jcs-web-mode)

  ;; NOTE(jenchieh): Get back highlighting.
  ;;(font-lock-flush)
  ;;(font-lock-fontify-buffer)
  )

;;;###autoload
(defun jcs-web-backward-delete-word ()
  "Web backward delete the word, fit PHP variable naming."
  (interactive)

  (backward-delete-char 1)

  (when (and (not (jcs-current-whitespacep))
             (not (jcs-current-char-equal-p "$"))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word)))

;;;###autoload
(defun jcs-web-backward-delete-word-capital ()
  "Web backward delete word capital, fit PHP variable naming."
  (interactive)

  (backward-delete-char 1)

  (when (and (not (jcs-current-whitespacep))
             (not (jcs-current-char-equal-p "$"))
             (not (jcs-current-char-uppercasep))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word-capital))

  (when (and (jcs-current-char-uppercasep)
             (not (jcs-current-char-equal-p "$")))
    (backward-delete-char 1)))

;;========================================
;;      JCS Format File
;;----------------------------------

;;;###autoload
(defun jcs-web-indent-region ()
  "Indent region for `web-mode'."
  (interactive)
  (save-excursion
    (let ((startLineNum -1)
          (startLineNum2 -1)
          (endLineNum -1)
          (endLineNum2 -1)
          ;; Turn off auto truncate when doing this command.
          (jcs-web-auto-truncate-lines nil))
      (setq endLineNum (string-to-number (format-mode-line "%l")))

      (goto-char (region-beginning))
      (setq startLineNum (string-to-number (format-mode-line "%l")))

      (exchange-point-and-mark)

      (goto-char (region-end))
      (setq endLineNum2 (string-to-number (format-mode-line "%l")))

      (goto-char (region-beginning))
      (setq startLineNum2 (string-to-number (format-mode-line "%l")))

      (deactivate-mark)

      (with-no-warnings
        (goto-line startLineNum)
        (previous-line 1))

      (while (and (<= (string-to-number (format-mode-line "%l")) endLineNum))
        (jcs-web-smart-indent-down)
        (end-of-line))

      (with-no-warnings
        (goto-line endLineNum2)
        (next-line 1))

      (while (and (>= (string-to-number (format-mode-line "%l")) startLineNum2))
        (jcs-web-smart-indent-up)
        (end-of-line)))))

;;;###autoload
(defun jcs-web-format-document ()
  "Indent the whoe document line by line instead of indent it \
once to the whole document.  For `web-mode'."
  (interactive)
  (save-excursion
    (save-window-excursion
      (let ((max-linum (line-number-at-pos (point-max)))
            ;; Start with first line.
            (current-linum (line-number-at-pos (point-min)))
            ;; Turn off auto truncate when doing this command.
            (jcs-web-auto-truncate-lines nil))
        ;; Start with first line.
        (goto-char (point-min))

        ;; Don't forget to indent the first line too.
        (jcs-web-smart-indent-down)
        (jcs-web-smart-indent-up)

        (while (< current-linum max-linum)
          (jcs-web-smart-indent-down)
          (setq current-linum (line-number-at-pos (point))))))))

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
    (previous-line 1))

  ;; Check if do truncate lines?
  (when jcs-web-auto-truncate-lines
    (jcs-web-truncate-lines-by-face)))

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
    (next-line 1))

  ;; Check if do truncate lines?
  (when jcs-web-auto-truncate-lines
    (jcs-web-truncate-lines-by-face)))

;;;###autoload
(defun jcs-web-right ()
  "Arrow right key for Web mode."
  (interactive)
  (right-char 1)
  ;; Check if do truncate lines?
  (when jcs-web-auto-truncate-lines
    (jcs-web-truncate-lines-by-face)))

;;;###autoload
(defun jcs-web-left ()
  "Arrow left key for Web mode."
  (interactive)
  (left-char 1)
  ;; Check if do truncate lines?
  (when jcs-web-auto-truncate-lines
    (jcs-web-truncate-lines-by-face)))

;;;###autoload
(defun jcs-web-return ()
  "Return key for `web-mode'."
  (interactive)

  (let (;; NOTE(jenchieh): Disable auto truncate lines effect
        ;; before save.
        (jcs-web-auto-truncate-lines nil)
        ;; NOTE(jenchieh): check if need a line between a pair.
        (line-between-pair nil)
        ;; NOTE(jenchieh): column before and after column.
        (line-before-column -1)
        (line-after-column -1))
    (save-excursion
      (ignore-errors
        (when (and (jcs-first-backward-char-p ">")
                   (jcs-first-forward-char-p "<"))
          (setq line-between-pair t))))

    ;; Get column before we do line break.
    (save-excursion
      (jcs-goto-first-char-in-line)
      (setq line-before-column (current-column)))

    ;; Call defulat line break function first.
    (jcs-smart-context-line-break)

    ;; Get column after line break and indent correctly.
    (save-excursion
      (jcs-goto-first-char-in-line)
      (setq line-after-column (current-column)))

    (when (and
           ;; Check if need a line between a pair tag.
           line-between-pair
           ;; Check if they tag are the same level.
           (= line-before-column line-after-column))
      ;; Fix curly bracket not indent correctly.
      (jcs-smart-context-line-break)
      (jcs-web-smart-indent-down)
      (jcs-web-smart-indent-up)
      (jcs-web-smart-indent-up))))

;;---------------------------------------------
;; Save
;;---------------------------------------------

;;;###autoload
(defun jcs-web-save-buffer ()
  "Save buffer in `web-mode'."
  (interactive)
  (let (;; NOTE(jenchieh): Disable auto truncate lines effect
        ;; before save.
        (jcs-web-auto-truncate-lines nil)
        ;; Use to record down if the cursor was beginning of
        ;; the line before we do the formatting document action.
        (tmp-was-beginning-of-line nil))
    ;; Record down if was beginning of line before formatting
    ;; whole document.
    (when (jcs-is-beginning-of-line-p)
      (setq tmp-was-beginning-of-line t))

    ;; NOTE(jenchieh): Do the formatting.
    ;; This will mess up the indentation that is
    ;; why we need to record down before we do the
    ;; formatting document action.
    (jcs-web-format-document)

    ;; Decide if we need to indent or not.
    (when (not tmp-was-beginning-of-line)
      (indent-for-tab-command))
    (jcs-untabify-save-buffer)))

;;;###autoload
(defun jcs-css-save-buffer ()
  "Save buffer in `css-mode'."
  (interactive)
  ;; NOTE(jenchieh): after using this, I think is better
  ;; if I bind this function/command to another key.
  ;;(com-css-sort-attributes-document)
  (jcs-untabify-save-buffer))

;;---------------------------------------------
;; Impatient Mode
;;---------------------------------------------

;;;###autoload
(defun jcs-httpd-start ()
  "Active real time editing with default port (`impatient-mode')."
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
             ;; Comment overwrite value face.
             ("\\(/\\*[a-zA-Z0-9 \n\t-.<>?,*'`@\"=_(){}:;&^%$#!~]*\\*/\\)" 1 'jcs-font-lock-comment-face t)
             )'end))
        jcs-css-value-modes)

  ;; Other faces.
  (setq-local font-lock-function-name-face '(:foreground "#17A0FB"))
  (setq-local font-lock-variable-name-face '(:foreground "#38EFCA")))

(defun jcs-init-web-faces ()
  "Initialize Web mode Faces Highlihgting."

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
