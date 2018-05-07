;; ========================================================================
;; $File: jcs-nav.el $
;; $Date: 2017-05-31 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


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
;; Navigating Blank Line
;;----------------------------------------------

;;;###autoload
(defun previous-blank-line ()
  "Move to the previous line containing nothing but whitespace."
  (interactive)
  (unless (ignore-errors (or (search-backward-regexp "^[ \t]*\n") t))
    (goto-char (point-min))))

;;;###autoload
(defun next-blank-line ()
  "Move to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (if (ignore-errors (or (search-forward-regexp "^[ \t]*\n") t))
      (forward-line -1)
    (goto-char (point-max))))

;;----------------------------------------------
;; Navigating Parentheses
;;----------------------------------------------

;;; TOPIC: Navigating Parentheses
;;; SOURCE: https://www.emacswiki.org/emacs/NavigatingParentheses

;; Record down the current searching character.
(setq jcs-current-search-char "")

(setq jcs-search-trigger-forward-char 0)
(setq jcs-search-trigger-backward-char 0)

;;;###autoload
(defun jcs-move-to-forward-a-char-recursive (ch)
  "Move forward to a character.
CH : character we target to move toward."
  (interactive "P")

  ;; If the last current is not the same as current character
  ;; reset the 'search wrapper' flag.
  (when (not (string= jcs-current-search-char ch))
    (setq jcs-search-trigger-forward-char 0)
    (setq jcs-current-search-char ch))

  (setq point-before-do-anything (point))

  (if (looking-at ch) (forward-char 1))
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
    (message "%s"
             (propertize (concat "Failing overwrap jcs-move-to-forward-a-char: "  ch)
                         'face
                         '(:foreground "cyan")))
    (setq jcs-search-trigger-forward-char 1)))

;;;###autoload
(defun jcs-move-to-backward-a-char-recursive (ch)
  "Move backward to a character.
CH : character we target to move toward."
  (interactive "P")

  ;; If the last current is not the same as current character
  ;; reset the 'search wrapper' flag.
  (when (not (string= jcs-current-search-char ch))
    (setq jcs-search-trigger-backward-char 0)
    (setq jcs-current-search-char ch))

  (setq point-before-do-anything (point))

  ;; so lets just search back part of the parenthesis
  (if (looking-at ch) (forward-char -1))
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
    (message "%s"
             (propertize (concat "Failing overwrap jcs-move-to-backward-a-char: " ch)
                         'face
                         '(:foreground "cyan")))
    (setq jcs-search-trigger-backward-char 1)))


(defun jcs-move-to-forward-a-char (ch)
  "Move forward to a character.
CH : character we target to move toward."
  (interactive "P")
  (ignore-errors
    (forward-char 1)
    (while (not (current-char-equal-p ch))
      (forward-char 1))))

(defun jcs-move-to-backward-a-char (ch)
  "Move backward to a character.
CH : character we target to move toward."
  (interactive "P")
  (ignore-errors
    (backward-char 1)
    (while (not (current-char-equal-p ch))
      (backward-char 1))))

(defun jcs-move-to-forward-a-char-do-recursive (ch &optional no-rec)
  "Move forward to a character and recusrive?
CH : character we target to move toward.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive "P")
  (if (equal no-rec t)
      (jcs-move-to-forward-a-char ch)
    (jcs-move-to-forward-a-char-recursive ch)))

(defun jcs-move-to-backward-a-char-do-recursive (ch &optional no-rec)
  "Move backward to a character and recusrive?
CH : character we target to move toward.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive "P")
  (if (equal no-rec t)
      (jcs-move-to-backward-a-char ch)
    (jcs-move-to-backward-a-char-recursive ch)))

(defun jcs-move-to-forward-a-word (word)
  "Move forward to a word.
WORD : word we target to move toward."
  (interactive "P")
  (ignore-errors
    (forward-word 1)
    (while (not (current-word-equal-p word))
      (forward-word 1))))

(defun jcs-move-to-backward-a-word (word)
  "Move backward to a word.
WORD : word we target to move toward."
  (interactive "P")
  (ignore-errors
    (backward-word 1)
    (while (not (current-word-equal-p word))
      (backward-word 1))))

;;;------------------------------------------------
;;; Move toggle Open and Close all kind of char.

(setq jcs-search-trigger-forward-open-close-char 0)
(setq jcs-search-trigger-backward-open-close-char 0)

;;;###autoload
(defun jcs-move-forward-open-close-epair (openChar closeChar)
  "Move forward to a open/close parenthesis."
  (interactive "P")

  ;; starting point
  (setq point-before-do-anything (point))

  (if (looking-at openChar) (forward-char 1))
  (ignore-errors (while (not (looking-at openChar)) (forward-char 1)))
  (setq point-after-look-open-char (point))

  ;; back to where it start
  (goto-char point-before-do-anything)

  (if (looking-at closeChar) (forward-char 1))
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
    (goto-char point-after-look-open-char)
    )

  (if (= jcs-search-trigger-forward-open-close-char 1)
      (progn
        (beginning-of-buffer)
        (setq jcs-search-trigger-forward-open-close-char 0)
        (jcs-move-forward-open-close-epair openChar closeChar)
        )
    )

  (if (and (= point-after-look-open-char point-end-of-buffer)
           (= point-after-look-close-char point-end-of-buffer))
      (progn
        (goto-char point-before-do-anything)
        (message "%s"
                 (propertize (concat "Failing overwrap jcs-move-forward-open-close-epair: '"  openChar "' and '" closeChar "'")
                             'face
                             '(:foreground "cyan")))
        (setq jcs-search-trigger-forward-open-close-char 1)
        )
    )
  )

;;;###autoload
(defun jcs-move-backward-open-close-epair (openChar closeChar)
  "Move backward to a open/close parenthesis."
  (interactive "P")

  (setq point-before-do-anything (point))

  (if (looking-at openChar) (forward-char -1))
  (ignore-errors  (while (not (looking-at openChar)) (backward-char 1)))
  (setq point-after-look-open-char (point))

  ;; back to where it start
  (goto-char point-before-do-anything)

  (if (looking-at closeChar) (forward-char -1))
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
    (goto-char point-after-look-close-char)
    )

  (if (= jcs-search-trigger-backward-open-close-char 1)
      (progn
        (end-of-buffer)
        (setq jcs-search-trigger-backward-open-close-char 0)
        (jcs-move-backward-open-close-epair openChar closeChar)
        )
    )

  (if (and (= point-after-look-open-char point-beginning-of-buffer)
           (= point-after-look-close-char point-beginning-of-buffer))
      (progn
        (goto-char point-before-do-anything)
        (message "%s"
                 (propertize (concat "Failing overwrap jcs-move-forward-open-close-epair: '"  openChar "' and '" closeChar "'")
                             'face
                             '(:foreground "cyan")))
        (setq jcs-search-trigger-backward-open-close-char 1)
        )
    )
  )

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
  "Move forward to a open/close sqr parenthesis."
  (interactive)
  (jcs-move-forward-open-close-epair "[[]" "]"))

;;;###autoload
(defun jcs-move-backward-open-close-sqrParen ()
  "Move backward to a open/close sqr parenthesis."
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
  "Move forward to a double quotation mark.
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
  "Move forward to a double quotation mark.
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
  "Move forward to a open parenthesis.
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
  "Move forward to a close parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ")" no-rec))

;;;------------------------------------------------
;;; Open Sqr Parenthesis

;;;###autoload
(defun jcs-move-forward-open-sqrParen (&optional no-rec)
  "Move forward to a open sqr parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "[[]" no-rec))

;;;###autoload
(defun jcs-move-backward-open-sqrParen (&optional no-rec)
  "Move forward to a open sqr parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "[[]" no-rec))

;;;------------------------------------------------
;;; Close Sqr Parenthesis

;;;###autoload
(defun jcs-move-forward-close-sqrParen (&optional no-rec)
  "Move forward to a close sqr parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "]" no-rec))

;;;###autoload
(defun jcs-move-backward-close-sqrParen (&optional no-rec)
  "Move forward to a close sqr parenthesis.
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
  "Move forward to a open curly parenthesis.
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
  "Move forward to a close curly parenthesis.
as NO-REC : recursive? (Default: do recusrive method)"
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "}" no-rec))


;;----------------------------------------------
;; Navigate Windows
;;----------------------------------------------

;;;
;; URL(jenchieh): https://www.emacswiki.org/emacs/WindowNavigation
;; Author: ChrisDone
;;
(defun jcs-jump-to-window (buffer-name)
  "Jump to window.
BUFFER-NAME : buffer name."
  (interactive "bEnter buffer to jump to: ")
  (let ((visible-buffers (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list)))
        window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have visible window" buffer-name)
      (setq window-of-buffer
            (delq nil (mapcar '(lambda (window)
                                 (if (equal buffer-name (buffer-name (window-buffer window)))
                                     window nil)) (window-list))))
      (select-window (car window-of-buffer)))))


;;----------------------------------------------
;; Navigate Search
;;----------------------------------------------

(defun jcs-search-forward-at-point ()
  "Search the word at point forward."
  (interactive)
  (isearch-forward-symbol-at-point))

(defun jcs-search-backword-at-point ()
  "Search the word at point backward."
  (interactive)
  (isearch-forward-symbol-at-point))
