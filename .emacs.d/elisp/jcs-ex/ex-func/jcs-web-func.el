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
  )

;;;###autoload
(defun jcs-web-yank ()
  "Yank in web-mode. No idea why, yank function just need to get
wrap by another function..."
  (interactive)

  ;; if region, delete the region first.
  (if (use-region-p)
      ;; NOTE(jayces): `kill-region' will copy the word.
      ;; Use `delete-region' instead, this will not copy
      ;; the word.
      (delete-region (region-beginning) (region-end))
    )

  ;; then paste it.
  (yank)

  ;; NOTE(jenchieh): Unknown reason that web-mode will
  ;; get disable...
  (web-mode)

  ;; NOTE(jenchieh): Get back highlighting.
  ;;(font-lock-flush)
  )

;;;###autoload
(defun jcs-web-backward-delete-word ()
  "Web backward delete the word, fit PHP variable naming."
  (interactive)

  (backward-delete-char 1)

  (if (and (not (current-whitespacep))
           (not (current-char-equal-p "$"))
           (jcs-current-char-a-wordp))
      (jcs-web-backward-delete-word)
    )
  )

;;;###autoload
(defun jcs-web-backward-delete-word-capital ()
  "Web backward delete word capital, fit PHP variable naming."
  (interactive)

  (backward-delete-char 1)

  (if (and (not (current-whitespacep))
           (not (current-char-equal-p "$"))
           (not (jcs-current-char-uppercasep))
           (jcs-current-char-a-wordp))
      (jcs-web-backward-delete-word-capital)
    )

  (if (and (jcs-current-char-uppercasep)
           (not (current-char-equal-p "$")))
      (backward-delete-char 1))
  )

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
      (jcs-smart-indent-down)
      (end-of-line))

    (goto-line endLineNum2)
    (next-line 1)

    (while (and (>= (string-to-number (format-mode-line "%l")) startLineNum2))
      (jcs-smart-indent-up)
      (end-of-line))
    )
  )

;;;###autoload
(defun jcs-web-format-document ()
  "Indent the whoe document line by line instead of indent it
once to the whole document. For `web-mode'."
  (interactive)

  ;;;
  ;; `save-current-buffer' saves the current buffer, so that
  ;; you can switch to another buffer without having to remember to switch back.
  ;;
  ;; `save-excursion' saves the current buffer and its current
  ;; point and mark too, so you can move point without having
  ;; to remember to restore it.
  ;;
  ;; `save-restriction' saves the restriction so that you can
  ;; narrow or widen it without having to remember to restore it.
  ;;
  ;; `save-window-excursion' saves the complete configuration
  ;; of all the windows on the frame, except for the value of
  ;; point in the current buffer.
  ;;
  ;; SOURCE: https://stackoverflow.com/questions/11596010/what-does-buffers-restrictions-mean-in-save-restriction
  ;;
  (save-excursion
    (save-window-excursion
      (end-of-buffer)
      (setq endPos (point))

      (beginning-of-buffer)

      (while (and (< (point) endPos))
        (jcs-smart-indent-down)
        (end-of-line)
        )
      )))

;;;###autoload
(defun jcs-web-format-region-or-document ()
  "Format the document if there are no region apply. For
`web-mode' we specificlly indent through the file line by
line instead of indent the whole file at once."
  (interactive)

  (if (use-region-p)
      (progn
        (call-interactively 'jcs-web-indent-region))
    (progn
      (call-interactively 'jcs-web-format-document))
    ))


;;---------------------------------------------
;; Save
;;---------------------------------------------

;;;###autoload
(defun jcs-web-save-buffer ()
  "Save buffer in `web-mode'."
  (interactive)
  (jcs-web-format-document)
  (visual-line-mode t)
  (jcs-save-buffer)
  )

;;;###autoload
(defun jcs-css-save-buffer ()
  "Save buffer in `css-mode'."
  (interactive)
  (jcs-css-sort-attributes-document)
  (jcs-save-buffer)
  )

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

;;;###autoload
(defun jcs-css-sort-attributes ()
  "Sort the CSS attributes for open and close curly parenthesis."
  (interactive)

  ;; record down the starting line.
  (let ((startLineNum (string-to-number (format-mode-line "%l"))))

    (save-excursion
      (save-window-excursion
        (ignore-errors
          (css-sort-attributes (point-min) (point-max)))))

    ;; make sure go back to the starting line.
    (goto-line startLineNum)
    (end-of-line))
  )

;;;###autoload
(defun jcs-css-sort-attributes-document ()
  "Sort all attributes for whole document."
  (interactive)

  ;; record down the starting line.
  (let ((startLineNum (string-to-number (format-mode-line "%l"))))

    (save-excursion
      (save-window-excursion
        (ignore-errors
          (beginning-of-buffer)

          (while (search-forward "}")
            (jcs-move-forward-close-curlyParen)
            ;; sort CSS attributes once.
            (css-sort-attributes (point-min) (point-max)))
          )))

    ;; make sure go back to the starting line.
    (goto-line startLineNum)
    (end-of-line))
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-web-func.el file
