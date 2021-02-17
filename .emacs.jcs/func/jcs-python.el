;;; jcs-python.el --- Python related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'python-mode)

;;;###autoload
(defun jcs-py-indent-region ()
  "Indent region for `python-mode'."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((ed-ln-num (line-number-at-pos)) (st-ln-num -1)
            (ed-ln-num-2 -1) (st-ln-num-2 -1))
        (goto-char (region-beginning))
        (setq st-ln-num (line-number-at-pos))

        (exchange-point-and-mark)

        (goto-char (region-end))
        (setq ed-ln-num-2 (line-number-at-pos))

        (goto-char (region-beginning))
        (setq st-ln-num-2 (line-number-at-pos))

        (deactivate-mark)

        (jcs-goto-line st-ln-num)
        (jcs-previous-line)

        (while (and (<= (line-number-at-pos) ed-ln-num))
          (jcs-py-indent-down)
          (end-of-line))

        (jcs-goto-line ed-ln-num-2)
        (jcs-next-line)

        (while (and (>= (line-number-at-pos) st-ln-num-2))
          (jcs-py-indent-up)
          (end-of-line))))))

;;;###autoload
(defun jcs-py-format-document ()
  "Indent the whoe document line by line instead of indent it
once to the whole document. For `python-mode'."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((ed-ln-num (line-number-at-pos (point-max))))
        (goto-char (point-min))
        (while (and (<= (line-number-at-pos) ed-ln-num))
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


(defconst jcs-py-keywords
  '("class"
    "classmethod"
    "def"
    "from"
    "import"
    "staticmethod")
  "List of `python' keyword.")

(defun jcs-py-is-python-keyword (in-keyword)
  "Check if the current word is in the `python-keyword-list'."
  (jcs-contain-list-string jcs-py-keywords in-keyword))

;;
;; (@* "Templates" )
;;

;;;###autoload
(defun jcs-ask-python-template (type)
  (interactive
   (list (completing-read
          "Type of the Python template: " '("Class" "Plain"))))
  (pcase type
    ("Class" (jcs-insert-python-class-template))
    ("Plain" (jcs-insert-python-template))))

(provide 'jcs-python)
;;; jcs-python.el ends here
