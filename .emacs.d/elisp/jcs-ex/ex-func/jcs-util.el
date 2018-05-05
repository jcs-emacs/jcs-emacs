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

(defun jcs-get-timestamp-ver1 ()
  "Get timestamp version 1."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun jcs-get-timestamp-ver2 ()
  "Get timestamp version 2."
  (format-time-string "%Y/%m/%d %H:%M:%S"))

(defun jcs-get-timestamp-ver3 ()
  "Get timestamp version 3."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

;;---------------------------------------------
;; Make the data base on the format provided.
;;---------------------------------------------

(defun jcs-get-date()
  "Get date buffer in string type.."
  (format-time-string "%Y-%m-%d"))

;;---------------------------------------------
;; Insert year only.
;;---------------------------------------------
(defun jcs-get-year-only ()
  "Get Year buffer in string type."
  (format-time-string "%Y"))

;;---------------------------------------------
;; Make the time base on the format provided.
;;---------------------------------------------
(defun jcs-get-time()
  "Get time buffer in string type."
  (format-time-string "%H:%M:%S"))

;;---------------------------------------------
;; Organize Code.
;;---------------------------------------------

(defun jcs-keep-n-line-between (n-line)
  "Keep n line between the two line of code.  (Guarantee)
N-LINE : line between the two line of code"
  (save-excursion
    (let ((index 0))
      (while (< index n-line)
        (jcs-keep-one-line-between)
        ;; increament one.
        (setq index (1+ index))))))

;;;###autoload
(defun jcs-keep-one-line-between ()
  "Keep one line between the two line of code.
If you want to keep more than one line use
`jcs-keep-n-line-between' instead."
  (interactive)
  (if (current-line-empty-p)
      (progn
        (jcs-next-line)

        ;; Kill empty line until there is one line.
        (while (current-line-empty-p)
          (jcs-kill-whole-line)))
    (progn
      ;; Make sure have one empty line between.
      (insert "\n"))))

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
(defun jcs-insert-spaces-by-tab-width ()
  "Insert spaces depends on tab width configuration."
  (interactive)
  (let ((tmp-count 0))
    (while (< tmp-count tab-width)
      (insert " ")
      (setq tmp-count (1+ tmp-count)))))

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

(defun jcs-is-digit-string (c)
  "Check if C is a digit."
  (or (string= c "0")
      (string= c "1")
      (string= c "2")
      (string= c "3")
      (string= c "4")
      (string= c "5")
      (string= c "6")
      (string= c "7")
      (string= c "8")
      (string= c "9")))

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

(defun current-char-string-match-p (c)
  "Check the current character string match to 'C'."
  (let ((current-char-string (string (char-before))))
    (string-match current-char-string c)))

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
  "Goto beginning of line but ignore 'empty characters'(spaces/tabs)."
  (interactive)
  (back-to-indentation-or-beginning)
  (when (is-beginning-of-line-p)
    (back-to-indentation-or-beginning)))

;;;###autoload
(defun current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]\t]*$")))

;;;###autoload
(defun current-line-totally-empty-p ()
  "Current line empty with no spaces/tabs in there.  (absolute)."
  (and (is-beginning-of-line-p)
       (is-end-of-line-p)))

;;;###autoload
(defun is-end-of-line-p ()
  "Is at the end of line?"
  (save-excursion
    (let ((current-point nil)
          (end-line-point nil))
      (setq current-point (point))
      (end-of-line)
      (setq end-line-point (point))
      (= end-line-point current-point))))

;;;###autoload
(defun is-end-of-buffer-p ()
  "Is at the end of buffer?"
  (save-excursion
    (let ((current-point nil)
          (end-buffer-point nil))
      (setq current-point (point))
      (goto-char (point-max))
      (setq end-buffer-point (point))
      (= end-buffer-point current-point))))

;;;###autoload
(defun is-beginning-of-line-p ()
  "Is at the beginning of line?"
  (save-excursion
    (let ((current-point nil)
          (begin-line-point nil))
      (setq current-point (point))
      (beginning-of-line)
      (setq begin-line-point (point))
      (= begin-line-point current-point))))

;;;###autoload
(defun is-beginning-of-buffer-p ()
  "Is at the beginning of buffer?"
  (save-excursion
    (let ((current-point nil)
          (begin-buffer-point nil))
      (setq current-point (point))
      (goto-char (point-min))
      (setq begin-buffer-point (point))
      (= begin-buffer-point current-point))))

(defun is-current-file-empty-p ()
  "Check if the file a empty file."
  (and (is-beginning-of-buffer-p)
       (is-end-of-buffer-p)))

(defun jcs-get-current-line-integer ()
  "Get the current line as integer."
  (string-to-number (jcs-get-current-line-string)))

(defun jcs-get-current-line-string ()
  "Get the current line as string."
  (format-mode-line "%l"))

(defun is-current-line (line)
  "Is current line number this line?
LINE : number to check if current line this line?"
  (= (string-to-number (format-mode-line "%l")) line))

;;;###autoload
(defun is-at-start-of-line-p ()
  "Cursor is at the first character of this line?"
  (let ((current-point nil)
        (firstCharPoint nil))
    (save-excursion
      (setq current-point (point))
      (back-to-indentation)
      (setq firstCharPoint (point)))

    (= firstCharPoint current-point)))

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
@return { boolean } : true, is active. false, is not active."
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

;;;###autoload
(defun jcs-delete-region ()
  "Delete region by default value."
  (interactive)
  (delete-region (region-beginning) (region-end)))

;;---------------------------------------------
;; Comment
;;---------------------------------------------

(defun is-inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (nth 4 (syntax-ppss)))

;;;###autoload
(defun jcs-goto-start-of-the-comment ()
  "Go to the start of the comment."
  (interactive)
  (when (is-inside-comment-block-p)
    (backward-char 1)
    (jcs-goto-start-of-the-comment)))

;;;###autoload
(defun jcs-goto-end-of-the-comment ()
  "Go to the end of the comment."
  (interactive)
  (when (is-inside-comment-block-p)
    (backward-char -1)
    (jcs-goto-end-of-the-comment)))

;;---------------------------------------------
;; Face
;;---------------------------------------------

;;;###autoload
(defun jcs-what-face ()
  "Print out what face is current cursor on."
  (interactive)
  (let ((face (jcs-get-current-point-face)))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;###autoload
(defun jcs-get-current-point-face ()
  "Get current point's type face as string."
  (interactive)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    face))

(defun jcs-is-current-point-face (in-face)
  "Check if current face the same face as IN-FACE.
Returns, True if is the same as pass in face name string.
False, is not the same as pass in face name string.
IN-FACE : input face name as string."
  (let ((current-point-face (jcs-get-current-point-face)))

    ;; NOTE(jenchieh): If there is multiple faces at
    ;; a point, it will return a list instead of
    ;; string. Just get the first element which is
    ;; usually the foreground face.
    (when (listp current-point-face)
      (setq current-point-face (nth 0 current-point-face)))

    ;; Return result.
    (string= in-face current-point-face)))

;;---------------------------------------------
;; Font
;;---------------------------------------------

;;;###autoload
(defun jcs-list-font-list ()
  "List out all the fonts available."
  (interactive)
  (dolist (tmp-font (font-family-list))
    (message "%s" tmp-font)))

;;;###autoload
(defun jcs-change-font (inFont)
  "Choose a font and change that to the current font."
  (interactive
   (list (completing-read
          "Fonts: " (font-family-list))))

  ;; Change the font and keep the size.
  (if (font-existsp inFont)
      (progn
        (set-frame-font inFont t))
    (progn
      (jcs-error "Font you chose does not exists in current system, Please select other font."))))

(defun font-existsp (font)
  "Check if font exists?
FONT : font to check."
  (if (string-equal (describe-font font)
                    "No matching font being used")
      nil
    t))

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

;;---------------------------------------------
;; I/O
;;---------------------------------------------

(defun get-string-from-file (filePath)
  "Return filePath's file content.
TOPIC(jenchieh): Elisp: Read File Content as String or List of Lines
URL(jenchieh): http://ergoemacs.org/emacs/elisp_read_file_content.html

FILEPATH : file path."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun jcs-parse-ini (filePath)
  "Parse a .ini file.
FILEPATH : .ini file to parse."

  (let ((tmp-ini (get-string-from-file filePath))
        (tmp-ini-list '())
        (tmp-pair-list nil)
        (tmp-keyword "")
        (tmp-value "")
        (count 0))
    (setq tmp-ini (split-string tmp-ini "\n"))

    (dolist (tmp-line tmp-ini)
      ;; check not comment.
      (when (not (string-match-p "#" tmp-line))
        ;; Split it.
        (setq tmp-pair-list (split-string tmp-line "="))

        ;; Assign to temporary variables.
        (setq tmp-keyword (nth 0 tmp-pair-list))
        (setq tmp-value (nth 1 tmp-pair-list))

        ;; Check empty value.
        (when (and (not (string= tmp-keyword ""))
                   (not (equal tmp-value nil)))
          (let ((tmp-list '()))
            (add-to-list 'tmp-list tmp-keyword)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))
          (let ((tmp-list '()))
            (add-to-list 'tmp-list tmp-value)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))))
      (setq count (1+ count)))

    ;; return list.
    tmp-ini-list))

(defun jcs-get-properties (ini-list in-key)
  "Get properties data.  Search by key and returns value.
INI-LIST : ini list.  Please use this with/after using `jcs-parse-ini' function.
IN-KEY : key to search for value."
  (let ((tmp-index 0)
        (tmp-key "")
        (tmp-value "")
        (returns-value ""))

    (while (< tmp-index (length ini-list))
      ;; Get the key and data value.
      (setq tmp-key (nth tmp-index ini-list))
      (setq tmp-value (nth (1+ tmp-index) ini-list))

      ;; Find the match.
      (when (string= tmp-key in-key)
        ;; return data value.
        (setq returns-value tmp-value))

      ;; Search for next key word.
      (setq tmp-index (+ tmp-index 2)))

    ;; Found nothing, return empty string.
    returns-value))

;;---------------------------------------------
;; File
;;---------------------------------------------

(defun jcs-get-file-name ()
  "Get current file name."
  (file-name-nondirectory buffer-file-name))

(defun jcs-get-file-name-uppercase ()
  "Get current file name uppercase."
  (upcase (file-name-nondirectory buffer-file-name)))

(defun jcs-get-file-name-lowercase ()
  "Get current file name uppercase."
  (downcase (file-name-nondirectory buffer-file-name)))

(defun jcs-get-file-name-without-extension ()
  "Get current file name without extension."
  (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

(defun jcs-get-file-name-without-extension-uppercase ()
  "Get current file name without extension."
  (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

(defun jcs-get-file-name-without-extension-lowercase ()
  "Get current file name without extension."
  (downcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

;;---------------------------------------------
;; Directory
;;---------------------------------------------

(defvar jcs-vc-list '(".bzr"
                      ".cvs"
                      ".git"
                      ".hg"
                      ".svn")
  "Version Control list.")

(defun jcs-get-current-dir ()
  "Return the string of current directory."
  default-directory)

(defun jcs-file-directory-exists-p (filePath)
  "Return `True' if the directory/file exists.
Return `False' if the directory/file not exists.

FILEPATH : directory/file path.

NOTE(jenchieh): Weird this only works for directory not for
the file."
  (equal (file-directory-p filePath) t))

(defun jcs-is-vc-dir-p (dirPath)
  "Return `True' is version control diectory.
Return `False' not a version control directory.
DIRPATH : directory path."

  (let ((tmp-is-vc-dir nil))
    (dolist (tmp-vc-type jcs-vc-list)
      (let ((tmp-check-dir (concat dirPath "/" tmp-vc-type)))
        (when (jcs-file-directory-exists-p tmp-check-dir)
          (setq tmp-is-vc-dir t))))
    ;; Return retult.
    (equal tmp-is-vc-dir t)))

(defun jcs-up-one-dir-string (dirPath)
  "Go up one directory and return it directory string.
DIRPATH : directory path."
  ;; Remove the last directory in the path.
  (string-match "\\(.*\\)/" dirPath)
  (match-string 1 dirPath))

(defun jcs-vc-root-dir ()
  "Return version control root directory."
  (let ((tmp-current-dir (jcs-get-current-dir))
        (tmp-result-dir ""))
    (while (jcs-contain-string "/" tmp-current-dir)
      (when (jcs-is-vc-dir-p tmp-current-dir)
        ;; Return the result, which is the version control path
        ;; or failed to find the version control path.
        (setq tmp-result-dir tmp-current-dir))
      ;; go up one directory.
      (setq tmp-current-dir (jcs-up-one-dir-string tmp-current-dir)))
    ;; NOTE(jenchieh): if you do not like `/' at the end remove
    ;; concat slash function.
    (concat tmp-result-dir "/")))

;;---------------------------------------------
;; String
;;---------------------------------------------

(defun jcs-remove-string-by-substring (str substr)
  "Remove a certain string by a substring.
STR : string to want to be remove by SUBSTR.
SUBSTR : string you want to remove from STR."
  (s-replace substr "" str))

(defun jcs-replace-string (rp-tar rp-str str)
  "Replace a string from another string.
STR : main string to change.
RP-TAR : target string we want to remove from STR.
RP-STR : replace string will be set into STR."
  (s-replace rp-tar rp-str str))

(defun jcs-parse-bool (in-str)
  "Parse string to boolean type value.
IN-STR : input string to check if is a `true' value."
  (let ((tmp-bool nil))
    (when (or (string= in-str "True")
              (string= in-str "true")
              (string= in-str "t"))
      (setq tmp-bool t))
    ;; return result.
    tmp-bool))

(defun jcs-contain-string (in-sub-str in-str)
  "Check if a string is a substring of another string.
Return true if contain, else return false.
IN-SUB-STR : substring to see if contain in the IN-STR.
IN-STR : string to check by the IN-SUB-STR."
  (string-match-p (regexp-quote in-sub-str) in-str))

(defun jcs-string-has-no-lowercase (string)
  "Return true iff STRING has no lowercase
SOURCE(jenchieh): https://stackoverflow.com/questions/2129840/check-if-a-string-is-all-caps-in-emacs-lisp"
  (equal (upcase string) string))

(defun jcs-is-contain-list-string (inList inStr)
  "Check if a string contain in any string in the string list.
INLIST : list of string use to check if INSTR in contain one of
the string.
INSTR : string using to check if is contain one of the INLIST."
  (let ((tmp-found nil))
    (dolist (tmpStr inList)
      (when (jcs-contain-string tmpStr inStr)
        (setq tmp-found t)))
    (equal tmp-found t)))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-util.el file
