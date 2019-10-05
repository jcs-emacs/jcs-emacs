;;; jcs-python-func.el --- Python related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'python-mode)


(defun jcs-init-py-faces ()
  "Initialize Python mode faces highlighting."
  (let ((py-missing-modes '(python-mode)))
    (dolist (mode py-missing-modes)
      (font-lock-add-keywords
       mode
       '(;; NOTE: Fixed comment and string conflict.
         ("[^\"]\\(#[^\"\r\n]*\\)[^\"]" 1 'font-lock-comment-face t)
         ("[^\"]\\(\"[^\"]*\"\\)[^\"]" 1 'font-lock-string-face t)
         )'end))))

;;-----------------------------------------------------------
;;-----------------------------------------------------------

;;;###autoload
(defun jcs-py-indent-region ()
  "Indent region for `python-mode'."
  (interactive)
  (save-excursion
    (save-window-excursion
      (let ((endLineNum (line-number-at-pos))
            (startLineNum -1)
            (endLineNum2 -1)
            (startLineNum2 -1))

        (goto-char (region-beginning))
        (setq startLineNum (line-number-at-pos))

        (exchange-point-and-mark)

        (goto-char (region-end))
        (setq endLineNum2 (line-number-at-pos))

        (goto-char (region-beginning))
        (setq startLineNum2 (line-number-at-pos))

        (deactivate-mark)

        (jcs-goto-line startLineNum)
        (jcs-previous-line)

        (while (and (<= (line-number-at-pos) endLineNum))
          (jcs-py-indent-down)
          (end-of-line))

        (jcs-goto-line endLineNum2)
        (jcs-next-line)

        (while (and (>= (line-number-at-pos) startLineNum2))
          (jcs-py-indent-up)
          (end-of-line))
        ))))

;;;###autoload
(defun jcs-py-format-document ()
  "Indent the whoe document line by line instead of indent it
once to the whole document. For `python-mode'."
  (interactive)
  (save-excursion
    (save-window-excursion
      (let ((endLineNum (line-number-at-pos (point-max))))
        (goto-char (point-min))
        (while (and (<= (line-number-at-pos) endLineNum))
          (jcs-py-indent-down))))))

;;;###autoload
(defun jcs-py-format-region-or-document ()
  "Format the document if there are no region apply.
For `python-mode' we specificlly indent through the file line by line \
instead of indent the whole file at once."
  (interactive)
  (if (use-region-p)
      (call-interactively 'jcs-py-indent-region)
    (call-interactively 'jcs-py-format-document)))

;;;###autoload
(defun jcs-py-indent-up ()
  "Move to previous line and indent for `python-mode'."
  (interactive)
  (jcs-previous-line)
  (if (jcs-current-line-empty-p)
      (when (not (jcs-is-mark-active-or-region-selected-p))
        (py-indent-line-outmost))
    (when (jcs-is-infront-first-char-at-line-p)
      (back-to-indentation))))

;;;###autoload
(defun jcs-py-indent-down ()
  "Move to next line and indent for `python-mode'."
  (interactive)
  (jcs-next-line)
  (if (jcs-current-line-empty-p)
      (when (not (jcs-is-mark-active-or-region-selected-p))
        (py-indent-line-outmost))
    (when (jcs-is-infront-first-char-at-line-p)
      (back-to-indentation))))

;;;###autoload
(defun jcs-py-return ()
  "Return key for `python-mode'."
  (interactive)
  (call-interactively #'newline)
  (py-indent-line-outmost))


(defun jcs-py-safe-backward-delete-char ()
  "Backward delete char safely in `python-mode'."
  (when (jcs-py-check-backward-delete-space)
    (backward-delete-char 1)))

(defun jcs-py-check-backward-delete-space ()
  "Check able to backward delete the space."
  (and (not (jcs-is-beginning-of-line-p))
       ;; Make sure is not a tab.
       (jcs-current-char-equal-p " ")))


(defun jcs-py-check-first-char-of-line-is-keyword-p ()
  "Check the first character of the current line the keyword line."
  (let ((is-keyword nil))
    (save-excursion
      (back-to-indentation)
      (forward-char 1)
      (when (jcs-current-char-equal-p "@")
        (forward-char 1))
      (when (jcs-py-is-python-keyword (jcs-get-word-at-point))
        (setq is-keyword t)))
    is-keyword))


(defvar jcs-py-keywords '("class"
                          "classmethod"
                          "def"
                          "from"
                          "import"
                          "staticmethod")
  "List of `python' keyword.")

(defun jcs-py-is-python-keyword (in-keyword)
  "Check if the current word is in the `python-keyword-list'."
  (jcs-is-contain-list-string jcs-py-keywords in-keyword))


(defun jcs-py-do-doc-string ()
  "Check if should insert the doc string by checking only the comment character \
on the same line."
  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)

    (while (not (jcs-is-end-of-line-p))
      (forward-char 1)

      (when (and (not (jcs-current-whitespace-or-tab-p))
                 (not (jcs-current-char-equal-p "\"")))
        ;; return false.
        (setq do-doc-string nil)))

    ;; return true.
    do-doc-string))

(defun jcs-py-maybe-insert-codedoc ()
  "Insert common Python document/comment string."
  (interactive)
  ;; -- Officual
  ;; URL: https://www.python.org/dev/peps/pep-0008/
  ;; -- Google
  ;; URL: https://google.github.io/styleguide/pyguide.html
  ;; -- Hitchhiker's
  ;; URL: http://docs.python-guide.org/en/latest/writing/style/
  (let ((active-comment nil)
        (previous-line-not-empty nil)
        (dq-infront 0))
    ;; Count how many double quote infront.
    (save-excursion
      (when (jcs-current-char-equal-p "\"")
        (setq dq-infront (1+ dq-infront))
        (backward-char 1)
        (when (jcs-current-char-equal-p "\"")
          (setq dq-infront (1+ dq-infront)))))

    (cond ((= dq-infront 2)
           (insert "\"")
           (when (jcs-py-do-doc-string) (setq active-comment t)))
          ((= dq-infront 1)
           (let ((between-dq nil))
             (save-excursion
               (forward-char 1)
               (when (jcs-current-char-equal-p "\"")
                 (setq between-dq t)))
             (if between-dq (forward-char 1) (insert "\""))))
          (t (insert "\"\"") (backward-char 1)))

    (when active-comment
      (save-excursion
        ;; check if previous line empty.
        (jcs-previous-line)
        (when (not (jcs-current-line-empty-p))
          (setq previous-line-not-empty t))))

    (when previous-line-not-empty
      ;; OPTION: docstring option..
      (when (= jcs-py-doc-string-version 1) (insert "\n"))
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

        ;; Insert comment doc comment string.
        (jcs-insert-comment-style-by-current-line 1)))))


;;;###autoload
(defun jcs-ask-python-template (type)
  (interactive
   (list (completing-read
          "Type of the Python template: " '("Class"
                                            "Plain"))))
  (cond ((string= type "Class")
         (progn
           (jcs-insert-python-class-template)))
        ((string= type "Plain")
         (progn
           (jcs-insert-python-template)))))


(provide 'jcs-python-func)
;;; jcs-python-func.el ends here
