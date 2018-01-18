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

;;; Code:

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
  "Insert timestamp."
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun jcs-timestamp-ver2 ()
  "Insert timestamp."
  (insert (format-time-string "%Y/%m/%d %H:%M:%S")))

(defun jcs-timestamp-ver3 ()
  "Insert timestamp."
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;;---------------------------------------------
;; Make the data base on the format provided.
;;---------------------------------------------
(defun jcs-date()
  "Inset date."
  (insert (format-time-string "%Y-%m-%d")))

;;---------------------------------------------
;; Insert year only.
;;---------------------------------------------
(defun jcs-year-only ()
  "Insert Year."
  (insert (format-time-string "%Y")))

;;---------------------------------------------
;; Make the time base on the format provided.
;;---------------------------------------------
(defun jcs-time()
  "Insert time."
  (insert (format-time-string "%H:%M:%S")))


;;---------------------------------------------
;; Tab / Space
;;---------------------------------------------

(defun jcs-is-good-space-to-convert-to-tab-p ()
  "Check if current point a good space to convert for tab.
Generally you will have to check it four times."
  (and (not (is-beginning-of-line-p))
       (current-char-equal-p " ")))

(defun jcs-convert-space-to-tab (is-forward)
  "Convert space to tab if current point is space.
IS-FORWARD : forward check convert instead of backward."

  (save-excursion
    (let ((good-to-convert nil))
      (save-excursion
        (when (jcs-is-good-space-to-convert-to-tab-p)
          (if (equal is-forward t)
              (forward-char 1)
            (backward-char 1))
          (when (jcs-is-good-space-to-convert-to-tab-p)
            (if (equal is-forward t)
                (forward-char 1)
              (backward-char 1))
            (when (jcs-is-good-space-to-convert-to-tab-p)
              (if (equal is-forward t)
                  (forward-char 1)
                (backward-char 1))
              (when (jcs-is-good-space-to-convert-to-tab-p)
                (setq good-to-convert t))))))

      (when (equal good-to-convert t)
        (if (equal is-forward t)
            (progn
              (backward-delete-char -1)
              (backward-delete-char -1)
              (backward-delete-char -1)
              (backward-delete-char -1)
              (insert "\t"))
          (progn
            (backward-delete-char 1)
            (backward-delete-char 1)
            (backward-delete-char 1)
            (backward-delete-char 1)
            (insert "\t")))))))

;;;###autoload
(defun jcs-backward-convert-space-to-tab ()
  "Convert space to tab backward at point."
  (interactive)
  (jcs-convert-space-to-tab nil))

;;;###autoload
(defun jcs-forward-convert-space-to-tab ()
  "Convert space to tab forward at point."
  (interactive)
  (jcs-convert-space-to-tab t))

(defun jcs-convert-tab-to-space (is-forward)
  "Convert tab to space if current point is tab.
IS-FORWARD : forward conversion instead of backward conversion."
  (save-excursion
    (when (current-char-equal-p "\t")
      (if (equal is-forward t)
          (progn
            (backward-delete-char -1)
            (insert "    "))
        (progn
          (backward-delete-char 1)
          (insert "    "))))))

;;;###autoload
(defun jcs-backward-convert-tab-to-space ()
  "Convert tab to space backward at point."
  (interactive)
  (jcs-convert-tab-to-space nil))

;;;###autoload
(defun jcs-forward-convert-tab-to-space ()
  "Convert tab to space forward at point."
  (interactive)
  (jcs-convert-tab-to-space t))

;;;###autoload
(defun jcs-delete-space-infront-of-line ()
  "Delete tab/spaces before the first character in line."
  (interactive)
  (save-excursion
    (ignore-errors
      (jcs-goto-first-char-in-line)
      (push-mark-command nil)
      (beginning-of-line)
      (jcs-delete-region)
      (deactivate-mark))))

;;;###autoload
(defun jcs-delete-region ()
  "Delete region by default value."
  (interactive)
  (delete-region (region-beginning) (region-end)))

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

(defun jcs-current-char-a-wordp ()
  "Check if current character a usual letter."
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    (wordp current-char-char)))

(defun jcs-current-char-uppercasep()
  "Check if current character a uppercase character?"
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    (uppercasep current-char-char)))

(defun jcs-current-char-lowercasep ()
  "Check if current character a lowercase character?"
  (not (jcs-current-char-uppercasep)))

(defun current-whitespacep ()
  "Check if current character a whitespace character?"
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    (whitespacep current-char-char)))

(defun current-char-equal-p (c)
  "Check the current character equal to 'C'."
  (let ((current-char-string (string (char-before))))
    (string= current-char-string c)))

(defun jcs-get-current-char-byte ()
  "Get the current character as the 'byte'."
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    current-char-char))

(defun jcs-get-current-char-string ()
  "Get the current character as the 'string'."
  (let ((current-char nil)
        (current-char-string nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    current-char-string))

;;---------------------------------------------
;; Word
;;---------------------------------------------
(defun get-word-at-point ()
  "Get word at current cursor position."
  (interactive)
  (thing-at-point 'word))

(defun current-word-equal-p (str)
  "Check the current word equal to 'STR'."
  (string= (thing-at-point 'word) str))

;;---------------------------------------------
;; Line
;;---------------------------------------------

;;;###autoload
(defun jcs-goto-first-char-in-line ()
  "Goto beginning of line but ignore 'empty characters'(spaces/tab)."
  (interactive)
  (back-to-indentation-or-beginning)
  (when (is-beginning-of-line-p)
    (back-to-indentation-or-beginning)))

;;;###autoload
(defun current-line-empty-p ()
  "Current line empty, but accept spaces in there.  (not absolute)."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;;;###autoload
(defun current-line-totally-empty-p ()
  "Current line empty with no spaces in there.  (absolute)."
  (and (is-beginning-of-line-p)
       (is-end-of-line-p)))

;;;###autoload
(defun is-end-of-line-p ()
  "Is at the end of line?"
  (save-excursion
    (let ((currentPoint nil)
          (endLinePoint nil))
      (setq currentPoint (point))
      (end-of-line)
      (setq endLinePoint (point))
      (= endLinePoint currentPoint))))

;;;###autoload
(defun is-end-of-buffer-p ()
  "Is at the end of buffer?"
  (save-excursion
    (let ((currentPoint nil)
          (endBufferPoint nil))
      (setq currentPoint (point))
      (goto-char (point-max))
      (setq endBufferPoint (point))
      (= endBufferPoint currentPoint))))

;;;###autoload
(defun is-beginning-of-line-p ()
  "Is at the beginning of line?"
  (save-excursion
    (let ((currentPoint nil)
          (beginLinePoint nil))
      (setq currentPoint (point))
      (beginning-of-line)
      (setq beginLinePoint (point))
      (= beginLinePoint currentPoint))))

;;;###autoload
(defun is-beginning-of-buffer-p ()
  "Is at the beginning of buffer?"
  (save-excursion
    (let ((currentPoint nil)
          (beginBufferPoint nil))
      (setq currentPoint (point))
      (goto-char (point-min))
      (setq beginBufferPoint (point))
      (= beginBufferPoint currentPoint))))

(defun is-current-file-empty-p ()
  "Check if the file a empty file."
  (and (is-beginning-of-buffer-p)
       (is-end-of-buffer-p)))

;;;###autoload
(defun is-current-line (line)
  "Is current line number this line?
LINE : number to check if current line this line?"
  (= (string-to-number (format-mode-line "%l")) line))

;;;###autoload
(defun is-at-start-of-line-p ()
  "Cursor is at the first character of this line?"
  (let ((currentPoint nil)
        (firstCharPoint nil))
    (save-excursion
      (setq currentPoint (point))
      (back-to-indentation)
      (setq firstCharPoint (point)))

    (= firstCharPoint currentPoint)))

;;;###autoload
(defun is-met-first-char-at-line-p ()
  "Check current cursor point is after the first character at \
the current line.

@return { boolean } : true, infront of first character.
false, vice versa."
  (let ((isInfrontOfFirstChar t)
        (pointToCheck nil))
    (save-excursion
      (ignore-errors
        (setq pointToCheck (point))
        (beginning-of-line)

        (when (not (current-line-totally-empty-p))
          (forward-char 1))

        (while (<= (point) pointToCheck)
          (if (not (current-whitespacep))
              (setq isInfrontOfFirstChar nil))
          (forward-char 1))))

    (eq isInfrontOfFirstChar t)))

(defun jcs-empty-line-between-point (minPoint maxPoint)
  "Check if there is empty line between two point.
MINPOINT : smaller position.
MAXPOINT : larger position."
  (save-excursion
    (let ((there-is-empty-line nil))
      (when (>= minPoint maxPoint)
        (jcs-warning "Min point cannot be larger than max point..")
        ;; Return false.
        (equal there-is-empty-line t))

      (goto-char minPoint)
      (while (< (point) maxPoint)
        (when (current-line-empty-p)
          ;; Return true.
          (setq there-is-empty-line t)
          (equal there-is-empty-line t))
        (jcs-next-line))
      ;; Return false.
      (equal there-is-empty-line t))))

;;;###autoload
(defun safe-forward-char ()
  "Forward a char if not the end of the line."
  (if (not (is-beginning-of-line-p))
      (forward-char 1)))

;;;###autoload
(defun safe-backward-char ()
  "Backward a char if not the beginning of the line."
  (if (not (is-end-of-line-p))
      (backward-char 1)))

;;---------------------------------------------
;; Move between button.
;;---------------------------------------------
;;URL: https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html

;;;###autoload
(defun top-most-line ()
  "Move to top of the buffer."
  (interactive)
  ;; NOTE: 0 : top-most-line, -1 : bottom-most-line
  (move-to-window-line-top-bottom 0))

;;;###autoload
(defun bottom-most-line()
  "Move to bottom of the buffer."
  (interactive)
  ;; NOTE: 0 : top-most-line, -1 : bottom-most-line
  (move-to-window-line-top-bottom -1))

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
  "Is region active? But if `region-start' and `region-end' is at the same point this would not trigger.
Which normally that mark is active but does not move at all.

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

;;---------------------------------------------
;; Comment
;;---------------------------------------------

(defun is-inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (interactive)
  (nth 4 (syntax-ppss)))

(defun jcs-goto-start-of-the-comment ()
  "Go to the start of the comment."
  (interactive)
  (when (is-inside-comment-block-p)
    (backward-char 1)
    (jcs-goto-start-of-the-comment)))

(defun jcs-goto-end-of-the-comment ()
  "Go to the end of the comment."
  (interactive)
  (when (is-inside-comment-block-p)
    (backward-char -1)
    (jcs-goto-end-of-the-comment)))

;;---------------------------------------------
;; Face
;;---------------------------------------------

(defun jcs-what-face ()
  "Print out what face is current cursor on."
  (interactive)
  (let ((face (jcs-get-current-point-face)))
    (if face (message "Face: %s" face) (message "No face at %d" pos)))
  )

(defun jcs-get-current-point-face ()
  "Get current point's type face as string."
  (interactive)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    face))

;;---------------------------------------------
;; List
;;---------------------------------------------

(defun jcs-is-in-list-string (list str)
  "Check if a string in the string list.
LIST : list of strings.
STR : string to check if is inside the list of strings above."
  (let ((in-list nil))
    (dolist (tmp-str list)
      (if (string= tmp-str str)
          (setq in-list t)))
    in-list))

;;---------------------------------------------
;; Mode
;;---------------------------------------------

(defun jcs-is-current-major-mode-p (str)
  "Check if this major mode.
STR : major mode name."
  (string= major-mode str))

(defun jcs-is-minor-mode-enabled-p (mode-obj)
  "Check if this minor enabled in current buffer/file.
MODE-OBJ : mode object memory."
  (bound-and-true-p mode-obj))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-util.el file
