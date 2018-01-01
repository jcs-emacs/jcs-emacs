;; This is the start of jcs-python-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-python-func.el             -*- Emacs-Lisp -*-

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

;;; Code:

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

  (previous-line 1)

  (if (current-line-empty-p)
      (progn
        (if (not (is-mark-active-or-region-selected-p))
            (py-indent-line-outmost)))
    (progn
      (if (is-met-first-char-at-line-p)
          (back-to-indentation))))
  )

;;;###autoload
(defun jcs-py-indent-down ()
  "Move to next line and indent for `python-mode'."
  (interactive)

  (next-line 1)

  (if (current-line-empty-p)
      (progn
        (if (not (is-mark-active-or-region-selected-p))
            (py-indent-line-outmost)))
    (progn
      (if (is-met-first-char-at-line-p)
          (back-to-indentation))))
  )

;;;###autoload
(defun jcs-py-space ()
  "Space key for `python-mode'. If the current cursor position is
infront of the first character we indent the line instead of insert
the space."
  (interactive)

  (if (or (is-met-first-char-at-line-p)
          (is-beginning-of-line-p))
      (progn
        ;; insert 4 spaces.
        (insert "    "))
    (insert " ")))

;;;###autoload
(defun jcs-py-backspace ()
  "Backspace key for `python-mode'. If the current cursor position
is infront of the first character in the line we delete fource
spaces instead of `py-electric-backspace'."
  (interactive)

  (if (is-met-first-char-at-line-p)
      (progn
        (if (use-region-p)
            (progn
              (delete-region (region-beginning) (region-end)))
          (progn
            (if (and (not (is-beginning-of-line-p))
                     ;; Make sure is not a tab.
                     (current-char-equal-p " "))
                (progn
                  ;; delete four spaces
                  (backward-delete-char 1)
                  (backward-delete-char 1)
                  (backward-delete-char 1)
                  (backward-delete-char 1))
              (progn
                (backward-delete-char 1))))))
    (py-electric-backspace)))

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
        ["class"
         "classmethod"
         "def"
         "from"
         "import"
         "staticmethod"
         ])

  (setq index 0)
  (setq isKeyword nil)

  (while (< index (length jcs-python-keyword-list))
    (if (string= inKeyword (elt jcs-python-keyword-list index))
        (setq isKeyword t))
    (setq index (1+ index)))

  (eq isKeyword t))


(defun jcs-py-maybe-insert-codedoc ()
  "Insert common Python document/comment string.

-- Officual
URL(jenchieh): https://www.python.org/dev/peps/pep-0008/
-- Google
URL(jenchieh): https://google.github.io/styleguide/pyguide.html
-- Hitchhiker's
URL(jenchieh): http://docs.python-guide.org/en/latest/writing/style/"
  (interactive)

  (insert "\"")


  (let ((active-comment nil)
        (previous-line-not-empty nil))
    (save-excursion
      (backward-char 1)
      (if (current-char-equal-p "\"")
          (progn
            (backward-char 1)
            (if (current-char-equal-p "\"")
                (progn
                  (backward-char 1)
                  (if (not (current-char-equal-p "\""))
                      (setq active-comment t)
                    ))
              )))

      ;; check if previous line empty.
      (jcs-previous-line)
      (if (not (current-line-empty-p))
          (setq previous-line-not-empty t))
      )

    (if (and (equal active-comment t)
             (equal previous-line-not-empty t))
        (progn
          (if (= jcs-py-doc-string-version 1)
              (progn
                ;; OPTION(jenchieh): docstring option..
                (insert "\n")))
          (insert "Description here..\n")
          (insert "\"\"\"")

          (jcs-smart-indent-up)
          (jcs-smart-indent-down)
          (jcs-smart-indent-up)
          (end-of-line)

          ;; Check other comment type.
          ;; ex: param, returns, etc.
          (save-excursion
            ;; Goto the function line before insert doc string.
            (jcs-previous-line)
            (if (= jcs-py-doc-string-version 1)
                (progn
                  ;; OPTION(jenchieh): docstring option..
                  (jcs-previous-line)))

            ;; insert comment doc comment string.
            (jcs-insert-comment-style-by-current-line)
            )
          ))
    ))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-python-func.el file
