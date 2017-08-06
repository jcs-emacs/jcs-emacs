;; This is the start of jcs-oython-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-oython-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sat Aug 05 13:51:49 EST 2017>
;; Time-stamp: <2017-08-05 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-function is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-function is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the Python file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;;###autoload
(defun jcs-py-indent-region ()
  "Indent region for `python-mode'"
  (interactive)
  (save-excursion
    (save-window-excursion
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
        (jcs-py-indent-down)
        (end-of-line))

      (goto-line endLineNum2)
      (next-line 1)

      (while (and (>= (string-to-number (format-mode-line "%l")) startLineNum2))
        (jcs-py-indent-up)
        (end-of-line))
      )))

;;;###autoload
(defun jcs-py-format-document ()
  "Indent the whoe document line by line instead of indent it
once to the whole document. For `python-mode'."
  (interactive)
  (save-excursion
    (save-window-excursion
      (end-of-buffer)
      (setq endLineNum (string-to-number (format-mode-line "%l")))

      (beginning-of-buffer)
      (setq startLineNum (string-to-number (format-mode-line "%l")))

      (while (and (<= (string-to-number (format-mode-line "%l")) endLineNum))
        (jcs-py-indent-down)
        )
      )))

;;;###autoload
(defun jcs-py-format-region-or-document ()
  "Format the document if there are no region apply. For
`python-mode' we specificlly indent through the file line by
line instead of indent the whole file at once."
  (interactive)
  (if (use-region-p)
      (progn
        (call-interactively 'jcs-py-indent-region))
    (progn
      (call-interactively 'jcs-py-format-document))
    ))

;;;###autoload
(defun jcs-py-indent-up ()
  "Move to previous line and indent for `python-mode'."
  (interactive)

  (setq isCommentLine nil)

  (save-excursion
    (previous-line 1)

    ;; if not the empty line
    (if (not (current-line-totally-empty-p))
        (progn
          (back-to-indentation)
          (forward-char 1)
          ;; check if current line comment!
          (if (or (current-char-equal-p "#")
                  (jcs-py-check-first-char-of-line-is-keyword-p))
              (setq isCommentLine t))
          )))


  ;; Do nothing if is trying to highlight anything.
  (if (not (is-region-selected-p))
      ;; if not the comment line.
      (if (not (eq isCommentLine t))
          (progn
            (save-excursion
              (previous-line 1)
              ;; NOTE(jenchieh): this will break the
              ;; region select.
              (py-indent-line-outmost)))))

  (previous-line 1)

  ;; Is at the front of the first character, we automatically
  ;; move to the first character.
  (if (is-met-first-char-at-line-p)
      (back-to-indentation))
  )

;;;###autoload
(defun jcs-py-indent-down ()
  "Move to next line and indent for `python-mode'."
  (interactive)

  (setq isCommentLine nil)

  (save-excursion
    (next-line 1)

    ;; if not the empty line
    (if (not (current-line-totally-empty-p))
        (progn
          (back-to-indentation)
          (forward-char 1)
          ;; check if current line comment!
          (if (or (current-char-equal-p "#")
                  (jcs-py-check-first-char-of-line-is-keyword-p))
              (setq isCommentLine t))
          )))

  ;; Do nothing if is trying to highlight anything.
  (if (not (is-region-selected-p))
      ;; if not the comment line.
      (if (not (eq isCommentLine t))
          (progn
            (save-excursion
              (next-line 1)
              ;; NOTE(jenchieh): this will break the
              ;; region select.
              (py-indent-line-outmost)))))

  (next-line 1)

  ;; Is at the front of the first character, we automatically
  ;; move to the first character.
  (if (is-met-first-char-at-line-p)
      (back-to-indentation))
  )

;;;###autoload
(defun jcs-py-space ()
  "Space key for `python-mode'. If the current cursor position is
infront of the first character we indent the line instead of insert
the space."
  (interactive)

  (if (is-met-first-char-at-line-p)
      (py-indent-line-outmost)
    (insert " ")))

;;;###autoload
(defun jcs-py-check-first-char-of-line-is-keyword-p ()
  "Check the first character of the current line the keyword line.
SEE(jenchieh): keyword is listed below in
`jcs-py-is-python-keyword' function."
  (interactive)
  (setq isKeyword nil)

  (save-excursion
    (back-to-indentation)
    (forward-char 1)

    (if (current-char-equal-p "@")
        (forward-char 1))

    (if (jcs-py-is-python-keyword (get-word-at-point))
        (setq isKeyword t))
    )

  (eq isKeyword t))

;;;###autoload
(defun jcs-py-is-python-keyword (inKeyword)
  "Check if the current word is in the `jcs-python-keyword-list'.
vector list."
  (interactive)

  (setq jcs-python-keyword-list
        ["def"
         "staticmethod"
         "classmethod"
         ])

  (setq index 0)
  (setq isKeyword nil)

  (while (< index (length jcs-python-keyword-list))
    (if (string= inKeyword (elt jcs-python-keyword-list index))
        (setq isKeyword t))
    (setq index (1+ index)))

  (eq isKeyword t))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-oython-func.el file
