;;; jcs-comment.el --- Comment related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jcs-triple-char-comment-prefix-p (in-char)
  "Check if current line is a triple IN-CHAR style comment prefix.
For instance, `///', `---', etc."
  (save-excursion
    (let ((is-comment-prefix nil))
      (jcs-goto-first-char-in-line)
      (forward-char 1)
      (when (jcs-current-char-equal-p in-char)
        (forward-char 1)
        (when (jcs-current-char-equal-p in-char)
          (forward-char 1)
          (when (jcs-current-char-equal-p in-char)
            (setq is-comment-prefix t))))
      is-comment-prefix)))

(defun jcs-tripple-char-comment-prefix-at-current-point-p (in-char)
  "Check if the current point is triple IN-CHAR style comment prefix.
For instance, `///', `---', etc."
  (save-excursion
    (let ((is-comment-prefix-at-point nil))
      (when (jcs-current-char-equal-p in-char)
        (backward-char 1)
        (when (jcs-current-char-equal-p in-char)
          (backward-char 1)
          (when (jcs-current-char-equal-p in-char)
            (setq is-comment-prefix-at-point t))))
      is-comment-prefix-at-point)))

(defun jcs-do-doc-string-p ()
  "Check if able to insert docstring by checking current line with only comments."
  (save-excursion
    (let ((do-doc-string t))
      (jcs-goto-first-char-in-line)
      (while (not (jcs-is-end-of-line-p))
        (forward-char 1)
        (unless (jcs-current-char-string-match-p "[ \t*/]")
          ;; return false.
          (setq do-doc-string nil)
          do-doc-string))
      ;; return true.
      do-doc-string)))

(defun jcs-is-global-comment-doc-p (&optional pt)
  "Check if current comment block a global comment docstring relative to PT.
Return non-nil, if is a global comment docstring.
Return nil, if is NOT a global comment docstring."
  (save-excursion
    (when pt (goto-char pt))
    (jcs-goto-start-of-the-comment)
    (forward-char 2)
    (jcs-current-char-equal-p "*")))

;;;###autoload
(defun jcs-smart-context-line-break ()
  "Comment block."
  (interactive)
  (let ((start-of-global-comment-doc-p nil) (able-insert-docstring-p nil))
    ;; check if inside the comment block.
    (if (not (jcs-inside-comment-block-p))
        ;; else insert new line
        (newline-and-indent)

      (setq able-insert-docstring-p
            (and (save-excursion (search-backward "/*" (line-beginning-position) t))
                 (save-excursion (search-forward "*/" (line-end-position) t))
                 (jcs-do-doc-string-p)))

      ;; check the '/*' and '*/' on the same line?
      (if (not able-insert-docstring-p)
          (progn
            (insert "\n")
            (when (and (jcs-inside-comment-block-p) (jcs-is-global-comment-doc-p))
              (insert "* "))
            (indent-for-tab-command))
        (setq start-of-global-comment-doc-p t)

        (insert "\n* ")
        (indent-for-tab-command)

        (insert "\n")
        (indent-for-tab-command)

        ;; back one line up
        (jcs-previous-line)

        ;; Insert comment string here..
        (when (and (functionp 'jcs-docstring-modes-p) (jcs-docstring-modes-p))
          (jcs-insert-comment-string))

        ;; goto the end of line
        (end-of-line))

      (unless start-of-global-comment-doc-p
        (unless (jcs-is-global-comment-doc-p)
          (let (;; Check if the next line is the doc string comment line.
                (is-next-line-doc-string-comment-line nil)
                ;; When we break a line and there are still some content on the right.
                (line-have-content-on-right nil))
            (cond
             ((jcs-is-current-major-mode-p "csharp-mode")
              (save-excursion
                (when (not (jcs-current-line-comment-p))
                  (setq line-have-content-on-right t)))

              (save-excursion
                (jcs-previous-line)
                (if (jcs-vs-csharp-only-vs-comment-prefix-this-line-p)
                    (when line-have-content-on-right
                      (setq is-next-line-doc-string-comment-line t))
                  (when (jcs-vs-csharp-comment-prefix-p)
                    (setq is-next-line-doc-string-comment-line t))))

              ;; If we still not sure to insert docstring comment line yet.
              ;; Then we need to do deeper check.
              (unless is-next-line-doc-string-comment-line
                (let ((prev-line-vs-prefix nil) (next-line-vs-prefix nil))
                  (save-excursion
                    (jcs-previous-line)
                    (when (jcs-vs-csharp-comment-prefix-p)
                      (setq prev-line-vs-prefix t)))

                  ;; Only when previous have prefix.
                  (when prev-line-vs-prefix
                    (save-excursion
                      (jcs-next-line)
                      (when (jcs-vs-csharp-comment-prefix-p)
                        (setq next-line-vs-prefix t)))

                    (when (and prev-line-vs-prefix next-line-vs-prefix)
                      (setq is-next-line-doc-string-comment-line t)))))

              ;; Is doc-string comment line. Insert doc-string comment.
              (when is-next-line-doc-string-comment-line
                (insert "/// ")))
             ((jcs-is-current-major-mode-p "lua-mode")
              ;; Just insert for Lua.
              ;; Lua does not have issue like CSharp.
              (insert "-- ")))
            (indent-for-tab-command)))))))


;;;###autoload
(defun jcs-c-comment-pair ()
  "Auto pair c style comment block."
  (interactive)
  (let ((insert-pair nil))
    (when (jcs-current-char-equal-p "/")
      (setq insert-pair t))
    (insert "*")
    (save-excursion
      (when (and insert-pair (jcs-is-behind-last-char-at-line-p))
        (insert "*/")))))

(defun jcs-insert-comment-string ()
  "Insert comment document string."
  (save-excursion
    ;; Goto the function line before insert doc string.
    (jcs-next-line)
    (jcs-next-line)

    ;; insert comment doc comment string.
    (jcs-insert-comment-style-by-current-line "[{;]")))

;;;###autoload
(defun jcs-toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-comment-uncomment-region-or-line ()
  "Comment line or region, if there are region select then just comment region.
Otherwise comment line."
  (interactive)
  ;; check if there are region select
  (if (and mark-active (/= (point) (mark)))
      (comment-or-uncomment-region (region-beginning) (region-end))
    ;; else we just comment one single line.
    (jcs-toggle-comment-on-line)))

;;;###autoload
(defun jcs-comment-line ()
  "Comment the current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-uncomment-line ()
  "Uncomment the current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-comment-region-or-line ()
  "If no region selected then just comment the line."
  (interactive)
  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (comment-or-uncomment-region (region-beginning) (region-end))
    ;; else we just comment one single line.
    (jcs-comment-line)))

;;;###autoload
(defun jcs-uncomment-region-or-line ()
  "If no region selected then just comment the line."
  (interactive)
  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (comment-or-uncomment-region (region-beginning) (region-end))
    ;; else we just uncomment one single line.
    (jcs-uncomment-line)))

(provide 'jcs-comment)
;;; jcs-comment.el ends here
