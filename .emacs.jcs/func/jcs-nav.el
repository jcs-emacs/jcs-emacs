;;; jcs-nav.el --- Nagivation in file.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------
;; Move Between Line (Wrapper)
;;----------------------------------------------

;;;###autoload
(defun jcs-previous-line ()
  "Calling `previous-line' does not execute.
Just use this without remember Emacs Lisp function."
  (interactive)
  (previous-line 1))

;;;###autoload
(defun jcs-next-line ()
  "Calling `next-line' does not execute.
Just use this without remember Emacs Lisp function."
  (interactive)
  (next-line 1))


;;----------------------------------------------
;; Move Inside Line
;;----------------------------------------------

;;;###autoload
(defun jcs-back-to-indentation-or-beginning ()
  "Toggle between first character and beginning of line."
  (interactive)
  ;; SOURCE: https://www.emacswiki.org/emacs/BackToIndentationOrBeginning
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

;;;###autoload
(defun jcs-beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation

If you rather it go to beginning-of-line
first and to indentation on the next hit use
this version instead."
  (interactive)
  (if (bolp)
      (beginning-of-line)
    (back-to-indentation)))

;;;###autoload
(defun jcs-back-to-indentation ()
  "back to identation by checking first character in the line."
  (interactive)
  (beginning-of-line)
  (unless (jcs-current-line-totally-empty-p)
    (forward-char 1))
  (while (jcs-current-whitespace-or-tab-p)
    (forward-char 1))
  (backward-char 1))

;;;###autoload
(defun jcs-beginning-of-visual-line ()
  "JayCeS beginning of visual line."
  (interactive)
  (let ((first-line-in-non-truncate-line nil)
        (visual-line-column -1))
    ;; First, record down the beginning of visual line point.
    (save-excursion
      (call-interactively #'beginning-of-visual-line)
      (setq visual-line-column (current-column)))

    ;; Check if is the first line of non-truncate line mode.
    (when (= visual-line-column 0)
      (setq first-line-in-non-truncate-line t))

    (if first-line-in-non-truncate-line
        (call-interactively #'jcs-back-to-indentation-or-beginning)
      (let ((before-pnt (point)))
        (call-interactively #'beginning-of-visual-line)

        ;; If before point is the same as the current point.
        ;; We call regaulr `beginning-of-line' function.
        (when (= before-pnt (point))
          (call-interactively #'jcs-back-to-indentation-or-beginning))))))

;;;###autoload
(defun jcs-end-of-visual-line()
  "JayCeS end of visual line."
  (interactive)
  (let ((before-pnt (point)))
    (call-interactively #'end-of-visual-line)

    ;; If before point is the same as the current point.
    ;; We call regaulr `end-of-line' function.
    (when (= before-pnt (point))
      (call-interactively #'end-of-line))))

;;;###autoload
(defun jcs-beginning-of-line ()
  "JayCeS beginning of line."
  (interactive)
  (if truncate-lines
      (call-interactively #'jcs-back-to-indentation-or-beginning)
    (call-interactively #'jcs-beginning-of-visual-line)))

;;;###autoload
(defun jcs-end-of-line ()
  "JayCeS end of line."
  (interactive)
  (if truncate-lines
      (call-interactively #'end-of-line)
    (call-interactively #'jcs-end-of-visual-line)))


;;----------------------------------------------
;; Navigating Blank Line
;;----------------------------------------------

;;;###autoload
(defun jcs-previous-blank-line ()
  "Move to the previous line containing nothing but whitespaces or tabs."
  (interactive)
  (unless (ignore-errors (or (search-backward-regexp "^[ \t]*\n") t))
    (goto-char (point-min))))

;;;###autoload
(defun jcs-next-blank-line ()
  "Move to the next line containing nothing but whitespaces or tabs."
  (interactive)
  (forward-line)
  (if (ignore-errors (or (search-forward-regexp "^[ \t]*\n") t))
      (forward-line -1)
    (goto-char (point-max))))


;;----------------------------------------------
;; Character Navigation
;;----------------------------------------------

(defun jcs-move-to-forward-a-char (ch)
  "Move forward to a character.
CH : character we target to move toward."
  (ignore-errors
    (forward-char 1)
    (while (and (not (jcs-current-char-equal-p ch))
                (not (jcs-is-end-of-buffer-p)))
      (forward-char 1))))

(defun jcs-move-to-backward-a-char (ch)
  "Move backward to a character.
CH : character we target to move toward."
  (ignore-errors
    (backward-char 1)
    (while (and (not (jcs-current-char-equal-p ch))
                (not (jcs-is-beginning-of-buffer-p)))
      (backward-char 1))))

(defun jcs-move-to-forward-a-word (word)
  "Move forward to a word.
WORD : word we target to move toward."
  (ignore-errors
    (forward-word 1)
    (while (and (not (jcs-current-word-equal-p word))
                (not (jcs-is-end-of-buffer-p)))
      (forward-word 1))))

(defun jcs-move-to-backward-a-word (word)
  "Move backward to a word.
WORD : word we target to move toward."
  (ignore-errors
    (backward-word 1)
    (while (and (not (jcs-current-word-equal-p word))
                (not (jcs-is-beginning-of-buffer-p)))
      (backward-word 1))))

;; TODO: The naming logic here is very weird..
;; Consider changing it.
(defun jcs-move-to-forward-a-char-do-recursive (ch &optional no-rec)
  "Move forward to a character and recusrive?
CH : character we target to move toward.
as NO-REC : recursive? (Default: do recusrive method)"
  (if (equal no-rec t)
      (jcs-move-to-forward-a-char ch)
    (jcs-move-to-forward-a-char-recursive ch)))

;; TODO: The naming logic here is very weird..
;; Consider changing it.
(defun jcs-move-to-backward-a-char-do-recursive (ch &optional no-rec)
  "Move backward to a character and recusrive?
CH : character we target to move toward.
as NO-REC : recursive? (Default: do recusrive method)"
  (if (equal no-rec t)
      (jcs-move-to-backward-a-char ch)
    (jcs-move-to-backward-a-char-recursive ch)))


;;----------------------------------------------
;; Symbol Navigation
;;----------------------------------------------

;;;###autoload
(defun jcs-backward-symbol (arg)
  (interactive "p")
  (forward-symbol (- arg)))

;;;###autoload
(defun jcs-forward-symbol (arg)
  (interactive "p")
  (forward-symbol arg))


;;----------------------------------------------
;; Navigating to a Character
;;----------------------------------------------

;;; TOPIC: Navigating Parentheses
;;; SOURCE: https://www.emacswiki.org/emacs/NavigatingParentheses

(defvar jcs-current-search-char ""
  "Record down the current searching character.")

(defvar jcs-search-trigger-forward-char 0
  "Trigger search forward character.")
(defvar jcs-search-trigger-backward-char 0
  "Trigger search backward character.")

(defun jcs-move-to-forward-a-char-recursive (ch)
  "Move forward to a character.
CH : character we target to move toward."

  (save-window-excursion
    ;; No matter what reset the backward trigger b/c we are doing
    ;; forward search now.
    (setq jcs-search-trigger-backward-char 0)

    ;; If the last current is not the same as current character
    ;; reset the 'search wrapper' flag.
    (when (not (string= jcs-current-search-char ch))
      (setq jcs-search-trigger-forward-char 0)
      (setq jcs-current-search-char ch))

    (let ((point-before-do-anything (point)))
      (when (looking-at ch)
        (forward-char 1))
      (ignore-errors (while (not (looking-at ch)) (forward-char 1)))

      ;; record down point max and point after look
      (setq point-after-look (point))
      (end-of-buffer)
      (setq point-end-of-buffer (point))

      ;; go back to search result.
      (goto-char point-after-look)

      (when (= jcs-search-trigger-forward-char 1)
        (beginning-of-buffer)
        (setq jcs-search-trigger-forward-char 0)
        (jcs-move-to-forward-a-char-recursive ch))

      (when (= point-after-look point-end-of-buffer)
        (goto-char point-before-do-anything)
        (setq jcs-search-trigger-forward-char 1)
        (message "%s"
                 (propertize (concat "Failing overwrap jcs-move-to-forward-a-char: " ch)
                             'face '(:foreground "cyan")))))))

(defun jcs-move-to-backward-a-char-recursive (ch)
  "Move backward to a character.
CH : character we target to move toward."

  (save-window-excursion
    ;; No matter what reset the forward trigger b/c we are doing
    ;; backward search now.
    (setq jcs-search-trigger-forward-char 0)

    ;; If the last current is not the same as current character
    ;; reset the 'search wrapper' flag.
    (when (not (string= jcs-current-search-char ch))
      (setq jcs-search-trigger-backward-char 0)
      (setq jcs-current-search-char ch))

    (let ((point-before-do-anything (point)))

      ;; so lets just search back part of the parenthesis
      (when (looking-at ch)
        (forward-char -1))
      (ignore-errors  (while (not (looking-at ch)) (backward-char 1)))

      ;; record down point min and point after look
      (setq point-after-look (point))
      (beginning-of-buffer)
      (setq point-beginning-of-buffer (point))

      ;; go back to search result.
      (goto-char point-after-look)

      (when (= jcs-search-trigger-backward-char 1)
        (end-of-buffer)
        (setq jcs-search-trigger-backward-char 0)
        (jcs-move-to-backward-a-char-recursive ch))

      (when (= point-after-look point-beginning-of-buffer)
        (goto-char point-before-do-anything)
        (setq jcs-search-trigger-backward-char 1)
        (message "%s"
                 (propertize (concat "Failing overwrap jcs-move-to-backward-a-char: " ch)
                             'face '(:foreground "cyan")))))))


;;;------------------------------------------------
;;; Move toggle Open and Close all kind of char.

(defvar jcs-search-trigger-forward-open-close-char 0
  "Trigger search forward open and close character.")
(defvar jcs-search-trigger-backward-open-close-char 0
  "Trigger search backward open and close character.")

(defun jcs-move-forward-open-close-epair (openChar closeChar)
  "Move forward to a open/close parenthesis."

  (save-window-excursion
    ;; No matter what reset the forward trigger b/c we are doing
    ;; backward search now.
    (setq jcs-search-trigger-backward-open-close-char 0)

    (let ((point-before-do-anything (point)))
      (when (looking-at openChar)
        (forward-char 1))
      (ignore-errors (while (not (looking-at openChar)) (forward-char 1)))
      (setq point-after-look-open-char (point))

      ;; back to where it start
      (goto-char point-before-do-anything)

      (when (looking-at closeChar)
        (forward-char 1))
      (ignore-errors (while (not (looking-at closeChar)) (forward-char 1)))
      (setq point-after-look-close-char (point))

      ;; record down point max and point after look
      (setq point-after-look (point))
      (end-of-buffer)
      (setq point-end-of-buffer (point))

      ;; back to where it start
      (goto-char point-before-do-anything)

      (if (> point-after-look-open-char point-after-look-close-char)
          (goto-char point-after-look-close-char)
        (goto-char point-after-look-open-char))

      (when (= jcs-search-trigger-forward-open-close-char 1)
        (beginning-of-buffer)
        (setq jcs-search-trigger-forward-open-close-char 0)
        (jcs-move-forward-open-close-epair openChar closeChar))

      (when (and (= point-after-look-open-char point-end-of-buffer)
                 (= point-after-look-close-char point-end-of-buffer))
        (goto-char point-before-do-anything)
        (setq jcs-search-trigger-forward-open-close-char 1)
        (message "%s"
                 (propertize (concat "Failing overwrap jcs-move-forward-open-close-epair: '" openChar "' and '" closeChar "'")
                             'face '(:foreground "cyan")))))))

(defun jcs-move-backward-open-close-epair (openChar closeChar)
  "Move backward to a open/close parenthesis."

  (save-window-excursion
    ;; No matter what reset the forward trigger b/c we are doing
    ;; backward search now.
    (setq jcs-search-trigger-forward-open-close-char 0)

    (let ((point-before-do-anything (point)))

      (when (looking-at openChar)
        (forward-char -1))
      (ignore-errors  (while (not (looking-at openChar)) (backward-char 1)))
      (setq point-after-look-open-char (point))

      ;; back to where it start
      (goto-char point-before-do-anything)

      (when (looking-at closeChar)
        (forward-char -1))
      (ignore-errors  (while (not (looking-at closeChar)) (backward-char 1)))
      (setq point-after-look-close-char (point))

      ;; record down point max and point after look
      (setq point-after-look (point))
      (beginning-of-buffer)
      (setq point-beginning-of-buffer (point))

      ;; back to where it start
      (goto-char point-before-do-anything)

      (if (> point-after-look-open-char point-after-look-close-char)
          (goto-char point-after-look-open-char)
        (goto-char point-after-look-close-char))

      (when (= jcs-search-trigger-backward-open-close-char 1)
        (end-of-buffer)
        (setq jcs-search-trigger-backward-open-close-char 0)
        (jcs-move-backward-open-close-epair openChar closeChar))

      (when (and (= point-after-look-open-char point-beginning-of-buffer)
                 (= point-after-look-close-char point-beginning-of-buffer))
        (goto-char point-before-do-anything)
        (setq jcs-search-trigger-backward-open-close-char 1)
        (message "%s"
                 (propertize (concat "Failing overwrap jcs-move-forward-open-close-epair: '" openChar "' and '" closeChar "'")
                             'face '(:foreground "cyan")))))))

;;;------------------------------------------------
;;; Move toggle Open and Close all kind of parenthesis.

;;;###autoload
(defun jcs-move-forward-open-close-paren ()
  "Move forward to a open/close parenthesis."
  (interactive)
  (jcs-move-forward-open-close-epair "(" ")"))

;;;###autoload
(defun jcs-move-backward-open-close-paren ()
  "Move backward to a open/close parenthesis."
  (interactive)
  (jcs-move-backward-open-close-epair "(" ")"))

;;;###autoload
(defun jcs-move-forward-open-close-sqrParen ()
  "Move forward to a open/close square parenthesis."
  (interactive)
  (jcs-move-forward-open-close-epair "[[]" "]"))

;;;###autoload
(defun jcs-move-backward-open-close-sqrParen ()
  "Move backward to a open/close square parenthesis."
  (interactive)
  (jcs-move-backward-open-close-epair "[[]" "]"))

;;;###autoload
(defun jcs-move-forward-open-close-curlyParen ()
  "Move forward to a open/close curly parenthesis."
  (interactive)
  (jcs-move-forward-open-close-epair "{" "}"))

;;;###autoload
(defun jcs-move-backward-open-close-curlyParen ()
  "Move backward to a open/close curly parenthesis."
  (interactive)
  (jcs-move-backward-open-close-epair "{" "}"))

;;;------------------------------------------------
;;; Single Quotation Mark

;;;###autoload
(defun jcs-move-forward-single-quot (&optional no-rec)
  "Move forward to a single quotation mark.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "'" no-rec))

;;;###autoload
(defun jcs-move-backward-single-quot (&optional no-rec)
  "Move backward to a single quotation mark.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "'" no-rec))

;;;------------------------------------------------
;;; Double Quotation Mark

;;;###autoload
(defun jcs-move-forward-double-quot (&optional no-rec)
  "Move forward to a double quotation mark.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "\"" no-rec))

;;;###autoload
(defun jcs-move-backward-double-quot (&optional no-rec)
  "Move backward to a double quotation mark.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "\"" no-rec))

;;;------------------------------------------------
;;; Open Parenthesis

;;;###autoload
(defun jcs-move-forward-open-paren (&optional no-rec)
  "Move forward to a open parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "(" no-rec))

;;;###autoload
(defun jcs-move-backward-open-paren (&optional no-rec)
  "Move backward to a open parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "(" no-rec))

;;;------------------------------------------------
;;; Close Parenthesis

;;;###autoload
(defun jcs-move-forward-close-paren (&optional no-rec)
  "Move forward to a close parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive ")" no-rec))

;;;###autoload
(defun jcs-move-backward-close-paren (&optional no-rec)
  "Move backward to a close parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ")" no-rec))

;;;------------------------------------------------
;;; Open Square Parenthesis

;;;###autoload
(defun jcs-move-forward-open-sqrParen (&optional no-rec)
  "Move forward to a open square parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "[[]" no-rec))

;;;###autoload
(defun jcs-move-backward-open-sqrParen (&optional no-rec)
  "Move backward to a open square parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "[[]" no-rec))

;;;------------------------------------------------
;;; Close Square Parenthesis

;;;###autoload
(defun jcs-move-forward-close-sqrParen (&optional no-rec)
  "Move forward to a close square parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "]" no-rec))

;;;###autoload
(defun jcs-move-backward-close-sqrParen (&optional no-rec)
  "Move backward to a close square parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "]" no-rec))

;;;------------------------------------------------
;;; Open Curly Parenthesis

;;;###autoload
(defun jcs-move-forward-open-curlyParen (&optional no-rec)
  "Move forward to a open curly parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "{" no-rec))

;;;###autoload
(defun jcs-move-backward-open-curlyParen (&optional no-rec)
  "Move backward to a open curly parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "{" no-rec))

;;;------------------------------------------------
;;; Close Curly Parenthesis

;;;###autoload
(defun jcs-move-forward-close-curlyParen (&optional no-rec)
  "Move forward to a close curly parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "}" no-rec))

;;;###autoload
(defun jcs-move-backward-close-curlyParen (&optional no-rec)
  "Move backward to a close curly parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "}" no-rec))

;;;------------------------------------------------
;;; Colon

;;;###autoload
(defun jcs-move-forward-colon (&optional no-rec)
  "Move forward to a colon.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive ":" no-rec))

;;;###autoload
(defun jcs-move-backward-colon (&optional no-rec)
  "Move backward to a colon.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ":" no-rec))

;;;------------------------------------------------
;;; Semicolon

;;;###autoload
(defun jcs-move-forward-semicolon (&optional no-rec)
  "Move forward to a semicolon.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive ";" no-rec))

;;;###autoload
(defun jcs-move-backward-semicolon (&optional no-rec)
  "Move backward to a semicolon.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ";" no-rec))

;;;------------------------------------------------
;;; Geater than sign

;;;###autoload
(defun jcs-move-forward-greater-than-sign (&optional no-rec)
  "Move forward to a greater than sign.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive ">" no-rec))

;;;###autoload
(defun jcs-move-backward-greater-than-sign (&optional no-rec)
  "Move backward to a greater than sign.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ">" no-rec))

;;;------------------------------------------------
;;; Less than sign

;;;###autoload
(defun jcs-move-forward-less-than-sign (&optional no-rec)
  "Move forward to a less than sign.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "<" no-rec))

;;;###autoload
(defun jcs-move-backward-less-than-sign (&optional no-rec)
  "Move backward to a less than sign.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "<" no-rec))

;;;------------------------------------------------
;;; Comma

;;;###autoload
(defun jcs-move-forward-comma (&optional no-rec)
  "Move forward to a comma.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "," no-rec))

;;;###autoload
(defun jcs-move-backward-comma (&optional no-rec)
  "Move backward to a comma.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "," no-rec))

;;;------------------------------------------------
;;; Period

;;;###autoload
(defun jcs-move-forward-period (&optional no-rec)
  "Move forward to a period.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "[.]" no-rec))

;;;###autoload
(defun jcs-move-backward-period (&optional no-rec)
  "Move backward to a period.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "[.]" no-rec))


(provide 'jcs-nav)
;;; jcs-nav.el ends here
