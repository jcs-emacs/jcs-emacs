;; This is the start of jcs-nav.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-nav.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2017>
;; Time-stamp: <2017-05-31 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-nav is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-nav is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;----------------------------------------------
;; Navigating Blank Line
;;----------------------------------------------

;; Navigation
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n"))

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1))

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
(defun jcs-move-to-forward-a-char (ch)
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
    (jcs-move-to-forward-a-char ch))

  (when (= point-after-look point-end-of-buffer)
    (goto-char point-before-do-anything)
    (message "%s"
             (propertize (concat "Failing overwrap jcs-move-to-forward-a-char: "  ch)
                         'face
                         '(:foreground "cyan")))
    (setq jcs-search-trigger-forward-char 1)))

;;;###autoload
(defun jcs-move-to-backward-a-char (ch)
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
    (jcs-move-to-backward-a-char ch))

  (when (= point-after-look point-beginning-of-buffer)
    (goto-char point-before-do-anything)
    (message "%s"
             (propertize (concat "Failing overwrap jcs-move-to-backward-a-char: " ch)
                         'face
                         '(:foreground "cyan")))
    (setq jcs-search-trigger-backward-char 1)))


(defun jcs-move-to-forward-a-char-no-prompt (ch)
  "Move forward to a character.
CH : character we target to move toward."
  (interactive "P")
  (forward-char 1)
  (while (not (current-char-equal-p ch))
    (forward-char 1)))

(defun jcs-move-to-backward-a-char-no-prompt (ch)
  "Move forward to a character.
CH : character we target to move toward."
  (interactive "P")
  (backward-char 1)
  (while (not (current-char-equal-p ch))
    (backward-char 1)))

(defun jcs-move-to-forward-a-word (word)
  "Move forward to a word.
WORD : word we target to move toward."
  (interactive "P")
  (forward-word 1)
  (while (not (current-word-equal-p word))
    (forward-word 1)))

(defun jcs-move-to-backward-a-word (word)
  "Move backward to a word.
WORD : word we target to move toward."
  (interactive "P")
  (backward-word 1)
  (while (not (current-word-equal-p word))
    (backward-word 1)))

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
(defun jcs-move-forward-single-quot (&optional no-prompt)
  "Move forward to a single quotation mark.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-forward-a-char-no-prompt "'")
    (jcs-move-to-forward-a-char "'")))

;;;###autoload
(defun jcs-move-backward-single-quot (&optional no-prompt)
  "Move forward to a double quotation mark.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-backward-a-char-no-prompt "'")
    (jcs-move-to-backward-a-char "'")))

;;;------------------------------------------------
;;; Double Quotation Mark

;;;###autoload
(defun jcs-move-forward-double-quot (&optional no-prompt)
  "Move forward to a double quotation mark.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-forward-a-char-no-prompt "\"")
    (jcs-move-to-forward-a-char "\"")))

;;;###autoload
(defun jcs-move-backward-double-quot (&optional no-prompt)
  "Move forward to a double quotation mark.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-backward-a-char-no-prompt "\"")
    (jcs-move-to-backward-a-char "\"")))

;;;------------------------------------------------
;;; Open Parenthesis

;;;###autoload
(defun jcs-move-forward-open-paren (&optional no-prompt)
  "Move forward to a open parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-forward-a-char-no-prompt "(")
    (jcs-move-to-forward-a-char "(")))

;;;###autoload
(defun jcs-move-backward-open-paren (&optional no-prompt)
  "Move forward to a open parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-backward-a-char-no-prompt "(")
    (jcs-move-to-backward-a-char "(")))

;;;------------------------------------------------
;;; Close Parenthesis

;;;###autoload
(defun jcs-move-forward-close-paren (&optional no-prompt)
  "Move forward to a close parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-forward-a-char-no-prompt ")")
    (jcs-move-to-forward-a-char ")")))

;;;###autoload
(defun jcs-move-backward-close-paren (&optional no-prompt)
  "Move forward to a close parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-backward-a-char-no-prompt ")")
    (jcs-move-to-backward-a-char ")")))

;;;------------------------------------------------
;;; Open Sqr Parenthesis

;;;###autoload
(defun jcs-move-forward-open-sqrParen (&optional no-prompt)
  "Move forward to a open sqr parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-forward-a-char-no-prompt "[[]")
    (jcs-move-to-forward-a-char "[[]")))

;;;###autoload
(defun jcs-move-backward-open-sqrParen (&optional no-prompt)
  "Move forward to a open sqr parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-backward-a-char-no-prompt "[[]")
    (jcs-move-to-backward-a-char "[[]")))

;;;------------------------------------------------
;;; Close Sqr Parenthesis

;;;###autoload
(defun jcs-move-forward-close-sqrParen (&optional no-prompt)
  "Move forward to a close sqr parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-forward-a-char-no-prompt "]")
    (jcs-move-to-forward-a-char "]")))

;;;###autoload
(defun jcs-move-backward-close-sqrParen (&optional no-prompt)
  "Move forward to a close sqr parenthesis.
as NO-PROMPT : no prompt?

nil = prompt failing search, then recursive move again.  (Default)
t = don't prompt, then no error handling."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-backward-a-char-no-prompt "]")
    (jcs-move-to-backward-a-char "]")))

;;;------------------------------------------------
;;; Open Curly Parenthesis

;;;###autoload
(defun jcs-move-forward-open-curlyParen (&optional no-prompt)
  "Move forward to a open curly parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-forward-a-char-no-prompt "{")
    (jcs-move-to-forward-a-char "{")))

;;;###autoload
(defun jcs-move-backward-open-curlyParen (&optional no-prompt)
  "Move forward to a open curly parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-backward-a-char-no-prompt "{")
    (jcs-move-to-backward-a-char "{")))

;;;------------------------------------------------
;;; Close Curly Parenthesis

;;;###autoload
(defun jcs-move-forward-close-curlyParen (&optional no-prompt)
  "Move forward to a close curly parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-forward-a-char-no-prompt "}")
    (jcs-move-to-forward-a-char "}")))

;;;###autoload
(defun jcs-move-backward-close-curlyParen (&optional no-prompt)
  "Move forward to a close curly parenthesis.
as NO-PROMPT : no prompt? (Default: prompt)

nil = prompt failed search, then recursive move again.  (Default)
t = don't prompt, no error handling, no recursive move again."
  (interactive)
  (if (equal no-prompt t)
      (jcs-move-to-backward-a-char-no-prompt "}")
    (jcs-move-to-backward-a-char "}")))


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


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-nav.el file
