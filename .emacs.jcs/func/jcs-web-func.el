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
    (when (not (jcs-current-char-string-match-p "[ \t]"))
      (if (and (jcs-is-default-face-p)
               (not (jcs-current-char-string-match-p "[\n><]")))
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

  (when (and (not (jcs-current-whitespace-or-tab-p))
             (not (jcs-current-char-equal-p "$"))
             (jcs-current-char-a-wordp))
    (jcs-web-backward-delete-word)))

;;;###autoload
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

;;;###autoload
(defun jcs-css-smart-indent-up ()
  "CSS smart indent up."
  (interactive)
  (jcs-previous-line)
  (save-excursion
    (indent-for-tab-command))

  (when (jcs-current-line-empty-p)
    (end-of-line)))

;;;###autoload
(defun jcs-css-smart-indent-down ()
  "CSS smart indent down."
  (interactive)
  (jcs-next-line)
  (save-excursion
    (indent-for-tab-command))

  (when (jcs-current-line-empty-p)
    (end-of-line)))

;;;###autoload
(defun jcs-css-return-key ()
  "CSS return key."
  (interactive)
  (if (jcs-is-end-of-line-p)
      (call-interactively #'jcs-smart-context-line-break)
    (progn
      (save-excursion
        (newline-and-indent))
      (forward-char 1))))

(defun jcs-init-css-faces ()
  "CSS Faces Highlighting."

  (defvar jcs-css-modes '(css-mode)
    "CSS mode we want to add it to highlight the face.")

  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("^[ \t]*\\([a-z0-9_-]*\\)[ \t]*:" 1 'jcs-css-type-face t)
             ("[ \t]*:[ \t]*\\(.*\\)[ \t]*;" 1 'jcs-css-value-face t)
             ;; Comment overwrite value face.
             ("\\(/\\*[a-zA-Z0-9 \n\t-.<>?,*'`@\"=_(){}:;&^%$#!~]*\\*/\\)" 1 'jcs-font-lock-comment-face t)
             ("[#]\\([a-z-A-Z0-9]*\\)[a-zA-Z0-9 \t>+~:]*[\n{]" 1 'jcs-css-id-face)
             ("[.]\\([a-z-A-Z0-9]*\\)[a-zA-Z0-9 \t>+~:]*[\n{]" 1 'jcs-css-class-face)
             )'end))
        jcs-css-modes)

  ;; Other faces.
  (setq-local font-lock-function-name-face '(:foreground "#17A0FB"))
  (setq-local font-lock-variable-name-face '(:foreground "#38EFCA")))

(defun jcs-init-web-faces ()
  "Initialize Web mode Faces Highlihgting."
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

;;-----------------------------------------------------------
;;-----------------------------------------------------------

;;;###autoload
(defun jcs-emmet-expand-line ()
  "Wrapper to JayCeS's version of `emmet-expand-line' function."
  (interactive)
  (if (jcs-is-current-point-face "link")
      (call-interactively #'goto-address-at-point)
    (call-interactively #'emmet-expand-line)))
