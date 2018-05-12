;; ========================================================================
;; $File: jcs-python-func.el $
;; $Date: 2017-08-05 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


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
        (jcs-py-indent-down))
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

  (if (jcs-current-line-empty-p)
      (progn
        (if (not (jcs-is-mark-active-or-region-selected-p))
            (py-indent-line-outmost)))
    (progn
      (if (jcs-is-met-first-char-at-line-p)
          (back-to-indentation))))
  )

;;;###autoload
(defun jcs-py-indent-down ()
  "Move to next line and indent for `python-mode'."
  (interactive)

  (next-line 1)

  (if (jcs-current-line-empty-p)
      (progn
        (if (not (jcs-is-mark-active-or-region-selected-p))
            (py-indent-line-outmost)))
    (progn
      (if (jcs-is-met-first-char-at-line-p)
          (back-to-indentation))))
  )


;;;###autoload
(defun jcs-py-real-space ()
  "Just insert a space!"
  (interactive)
  (insert " "))

;;;###autoload
(defun jcs-py-space ()
  "Space key for `python-mode'. If the current cursor position is
infront of the first character we indent the line instead of insert
the space."
  (interactive)

  (if (or (jcs-is-met-first-char-at-line-p)
          (jcs-is-beginning-of-line-p))
      (progn
        ;; insert 4 spaces.
        (insert "    "))
    (insert " ")))

;;;###autoload
(defun jcs-py-real-backspace ()
  "Just delete a char."
  (interactive)
  (backward-delete-char 1))

;;;###autoload
(defun jcs-py-backspace ()
  "Backspace key for `python-mode'. If the current cursor position
is infront of the first character in the line we delete fource
spaces instead of `py-electric-backspace'."
  (interactive)

  (if (jcs-is-met-first-char-at-line-p)
      (progn
        (if (use-region-p)
            (progn
              (delete-region (region-beginning) (region-end)))
          (progn
            (if (jcs-py-check-backward-delete-space)
                (progn
                  ;; delete four spaces
                  (jcs-py-safe-backward-delete-char)
                  (jcs-py-safe-backward-delete-char)
                  (jcs-py-safe-backward-delete-char)
                  (jcs-py-safe-backward-delete-char))
              (progn
                (backward-delete-char 1))))))
    (py-electric-backspace)))

;;;###autoload
(defun jcs-py-safe-backward-delete-char ()
  (when (jcs-py-check-backward-delete-space)
    (backward-delete-char 1)))

;;;###autoload
(defun jcs-py-check-backward-delete-space ()
  (and (not (jcs-is-beginning-of-line-p))
       ;; Make sure is not a tab.
       (jcs-current-char-equal-p " ")))

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

    (if (jcs-current-char-equal-p "@")
        (forward-char 1))

    (if (jcs-py-is-python-keyword (jcs-get-word-at-point))
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


(defun jcs-py-do-doc-string ()
  "Check if should insert the doc string by checking only \
comment character on the same line."

  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)

    (while (not (jcs-is-end-of-line-p))
      (forward-char 1)
      (when (and (not (jcs-current-char-equal-p " "))
                 (not (jcs-current-char-equal-p "\t"))
                 (not (jcs-current-char-equal-p "\"")))
        ;; return false.
        (setq do-doc-string nil)
        (equal do-doc-string t)))

    ;; return true.
    (equal do-doc-string t)))

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
      (when (jcs-current-char-equal-p "\"")
        (backward-char 1)
        (when (jcs-current-char-equal-p "\"")
          (backward-char 1)
          (when (not (jcs-current-char-equal-p "\""))
            (when (jcs-py-do-doc-string)
              (setq active-comment t)))))

      ;; check if previous line empty.
      (jcs-previous-line)
      (when (not (jcs-current-line-empty-p))
        (setq previous-line-not-empty t)))

    (when (and (equal active-comment t)
               (equal previous-line-not-empty t))
      (when (= jcs-py-doc-string-version 1)
        ;; OPTION(jenchieh): docstring option..
        (insert "\n"))
      (insert "Description here..\n")
      (insert "\"\"\"")

      (jcs-smart-indent-up)
      (jcs-smart-indent-down)
      (jcs-smart-indent-up)
      (end-of-line)

      ;; Check other comment type.
      ;; ex: param, returns, etc.
      (save-excursion
        ;; Move to `def' keyword in order to search all
        ;; the necessary info before inserting doc string.
        (jcs-move-to-backward-a-word "def")

        ;; insert comment doc comment string.
        (jcs-insert-comment-style-by-current-line 1)
        ))))
