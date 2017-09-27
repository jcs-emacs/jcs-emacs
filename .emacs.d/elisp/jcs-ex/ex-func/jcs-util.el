;; This is the start of jcs-util.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-util.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-util is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-util is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; All utilities put here.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;;---------------------------------------------
;; Make the time stamp base on the format
;; provided.
;;
;; Source:
;; -> https://www.emacswiki.org/emacs/InsertingTodaysDate
;;---------------------------------------------
(defun jcs-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun jcs-timestamp-ver2 ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S")))

;;---------------------------------------------
;; Make the data base on the format provided.
;;---------------------------------------------
(defun jcs-date()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;---------------------------------------------
;; Insert year only.
;;---------------------------------------------
(defun jcs-year-only ()
  (interactive)
  (insert (format-time-string "%Y")))

;;---------------------------------------------
;; Make the time base on the format provided.
;;---------------------------------------------
(defun jcs-time()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

;;---------------------------------------------
;; Character
;;---------------------------------------------

;; TOPIC: Check if a character (not string) is lowercase,
;; uppercase, alphanumeric?
;; SOURCE: https://stackoverflow.com/questions/27798296/check-if-a-character-not-string-is-lowercase-uppercase-alphanumeric
(defun wordp (c) (= ?w (char-syntax c)))
(defun lowercasep (c) (and (wordp c) (= c (downcase c))))
(defun uppercasep (c) (and (wordp c) (= c (upcase c))))
(defun whitespacep (c) (= 32 (char-syntax c)))

(defun jcs-current-char-uppercasep()
  "Check if current character a uppercase character?"
  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))
  (uppercasep current-char-char)
  )

(defun jcs-current-char-lowercasep ()
  "Check if current character a lowercase character?"
  (not (jcs-current-char-uppercasep))
  )

(defun current-whitespacep ()
  "Check if current character a whitespace character?"
  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))
  (whitespacep current-char-char))

(defun current-char-equal-p (c)
  "Check the current character equal to 'c'."
  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (string= current-char-string c))

(defun jcs-get-current-char-byte ()
  "Get the current character as the 'byte'."
  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))
  (current-char-char))

(defun jcs-get-current-char-string ()
  "Get the current character as the 'string'."
  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (current-char-string))


;;---------------------------------------------
;; Word
;;---------------------------------------------
(defun get-word-at-point ()
  "Get word at current cursor position."
  (interactive)
  (thing-at-point 'word))

;;---------------------------------------------
;; Line
;;---------------------------------------------

;;;###autoload
(defun current-line-empty-p ()
  "Current line empty, but accept spaces in there. (not absolute)"
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;;;###autoload
(defun current-line-totally-empty-p ()
  "Current line empty with no spaces in there. (absolute)"
  (and (is-beginning-of-line-p)
       (is-end-of-line-p)))

;;;###autoload
(defun is-end-of-line-p ()
  "Is at the end of line?"
  (save-excursion
    (setq currentPoint (point))
    (end-of-line)
    (setq endLinePoint (point)))
  (= endLinePoint currentPoint))

;;;###autoload
(defun is-end-of-buffer-p ()
  "Is at the end of buffer?"
  (save-excursion
    (setq currentPoint (point))
    (end-of-buffer)
    (setq endBufferPoint (point)))
  (= endBufferPoint currentPoint))

;;;###autoload
(defun is-beginning-of-line-p ()
  "Is at the beginning of line?"
  (save-excursion
    (setq currentPoint (point))
    (beginning-of-line)
    (setq beginLinePoint (point)))
  (= beginLinePoint currentPoint))

;;;###autoload
(defun is-beginning-of-buffer-p ()
  "Is at the beginning of buffer?"
  (save-excursion
    (setq currentPoint (point))
    (beginning-of-buffer)
    (setq beginBufferPoint (point)))
  (= beginBufferPoint currentPoint))

(defun is-current-file-empty-p ()
  "Check if the file a empty file."
  (and (is-beginning-of-buffer-p)
       (is-end-of-buffer-p))
  )

;;;###autoload
(defun is-current-line (line)
  "Is current line number this line?"
  (= (string-to-number (format-mode-line "%l")) line))

;;;###autoload
(defun is-at-start-of-line-p ()
  "Cursor is at the first character of this line?"
  (save-excursion
    (setq currentPoint (point))
    (back-to-indentation)
    (setq firstCharPoint (point)))

  (= firstCharPoint currentPoint))

;;;###autoload
(defun is-met-first-char-at-line-p ()
  "Check current cursor point is after the first character at
the current line.

@return { boolean } : true, infront of first character. false,
vice versa.
"
  (setq isInfrontOfFirstChar t)

  (save-excursion
    (setq pointToCheck (point))
    (beginning-of-line)

    (if (not (current-line-totally-empty-p))
        (forward-char 1))

    (while (<= (point) pointToCheck)
      (if (not (current-whitespacep))
          (setq isInfrontOfFirstChar nil))
      (forward-char 1))
    )

  (eq isInfrontOfFirstChar t))

;;;###autoload
(defun safe-forward-char ()
  "Forward a char if not the end of the line."
  (if (not (is-beginning-of-line-p))
      (forward-char 1))
  )

;;;###autoload
(defun safe-backward-char ()
  "Backward a char if not the beginning of the line."
  (if (not (is-end-of-line-p))
      (backward-char 1))
  )

;;---------------------------------------------
;; Move between button.
;;---------------------------------------------
;;URL: https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html

;;;###autoload
(defun top-most-line ()
  "Move to top of the buffer."
  (interactive)
  ;; NOTE: 0 : top-most-line, -1 : bottom-most-line
  (move-to-window-line-top-bottom 0)
  )

;;;###autoload
(defun bottom-most-line()
  "Move to bottom of the buffer."
  (interactive)
  ;; NOTE: 0 : top-most-line, -1 : bottom-most-line
  (move-to-window-line-top-bottom -1)
  )

;;---------------------------------------------
;; Mark
;;---------------------------------------------

;;;###autoload
(defun is-mark-active ()
  "Is mark active?

@return { boolean } : true, is active. false, is not active.
"
  (and mark-active
       (= (point) (mark))))

;;---------------------------------------------
;; Region
;;---------------------------------------------

;;;###autoload
(defun is-region-selected-p ()
  "Is region active? But if `region-start' and `region-end' is at
the same point this would not trigger. Which normally that mark
is active but does not move at all.

@return { boolean } : true, there is region selected. false, no
region selected.
"
  (use-region-p))

;;;###autoload
(defun is-mark-active-or-region-selected-p ()
  "Complete check if the region and the mark is active.

@return { boolean } : true, either region selected or mark is
active. false, there is no region selected and mark is not active.
"
  (or (is-region-selected-p)
      (is-mark-active)))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-util.el file
