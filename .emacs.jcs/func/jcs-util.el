;; ========================================================================
;; $File: jcs-util.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; All utilities put here.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;---------------------------------------------
;; Buffer
;;---------------------------------------------

;;;###autoload
(defun jcs-what-buffer ()
  "Message out what current window's buffer."
  (interactive)
  (message "Current buffer name: %s" (buffer-name)))

;;;###autoload
(defun jcs-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted.
They will be reverted though if they were modified outside Emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed.
SOURCE(jenchieh): https://emacs.stackexchange.com/questions/24459/revert-all-open-buffers-and-ignore-errors"
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers like *Messages*.
        (when (and filename
                   (not (buffer-modified-p buf)))
          (if (file-readable-p filename)
              ;; If the file exists and is readable, revert the buffer.
              (with-current-buffer buf
                (revert-buffer :ignore-auto :noconfirm :preserve-modes))
            ;; Otherwise, kill the buffer.
            (let (kill-buffer-query-functions) ; No query done when killing buffer
              (kill-buffer buf)
              ;;(message "Killed non-existing/unreadable file buffer: %s" filename)
              )))))
    ;;(message "Finished reverting buffers containing unmodified files.")
    ))

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

(defun jcs-get-year-only ()
  "Get Year buffer in string type."
  (format-time-string "%Y"))

(defun jcs-get-time()
  "Get time buffer in string type."
  (format-time-string "%H:%M:%S"))


;;;###autoload
(defun jcs-print-timestamps ()
  "Print out all the timestamps."
  (interactive)
  (message "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
  (message "=> Ver. %s" (jcs-get-timestamp-ver1))
  (message "=> Ver. %s" (jcs-get-timestamp-ver2))
  (message "=> Ver. %s" (jcs-get-timestamp-ver3))
  (message "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
  (jcs-do-after-log-action))

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
  (if (jcs-current-line-empty-p)
      (progn
        (jcs-next-line)

        ;; Kill empty line until there is one line.
        (while (jcs-current-line-empty-p)
          (jcs-kill-whole-line)))
    (progn
      ;; Make sure have one empty line between.
      (insert "\n"))))

;;---------------------------------------------
;; Tab / Space
;;---------------------------------------------

;;;###autoload
(defun jcs-delete-trailing-whitspace-current-line ()
  "Delete the trailing whitespace exist in current line."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((begin-of-line-point nil)
            (end-of-line-point nil))
        ;; Get beginning of line poinrt and end of line point.
        (beginning-of-line)
        (setq begin-of-line-point (point))
        (end-of-line)
        (setq end-of-line-point (point))

        (narrow-to-region begin-of-line-point end-of-line-point)
        (delete-trailing-whitespace)))))

(defun jcs-is-good-space-to-convert-to-tab-p ()
  "Check if current point a good space to convert for tab.
Generally you will have to check it four times."
  (and (not (jcs-is-beginning-of-line-p))
       (jcs-current-char-equal-p " ")))

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
    (when (jcs-current-char-equal-p "\t")
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
  (string-match-p "\^[0-9]'" c))

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

(defun jcs-current-whitespacep ()
  "Check if current character a whitespace character?"
  (let ((current-char nil)
        (current-char-string nil)
        (current-char-char nil))
    (setq current-char (char-before))
    (setq current-char-string (string current-char))
    (setq current-char-char (string-to-char current-char-string))
    (whitespacep current-char-char)))

(defun jcs-current-char-equal-p (c)
  "Check the current character equal to 'C'."
  (let ((current-char-string (string (char-before))))
    (string= current-char-string c)))

(defun jcs-current-char-string-match-p (c)
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

;;;###autoload
(defun jcs-goto-next-backward-char (&optional bnd-pt)
  "Goto the next backward character (not include space/tab).
BND-PT : limit point."
  (interactive)
  (let ((real-lmt-pt (point-min)))
    ;; If no limit point, default as `point-min'.
    (unless (equal bnd-pt nil)
      (setq real-lmt-pt bnd-pt))

    (forward-char -1)
    (while (and (>= (point) real-lmt-pt)
                (or (jcs-current-char-equal-p " ")
                    (jcs-current-char-equal-p "\t")
                    (jcs-is-beginning-of-line-p)))
      (forward-char -1))))

;;;###autoload
(defun jcs-goto-next-forward-char (&optional bnd-pt)
  "Goto the next forward character (not include space/tab).
BND-PT : boundary point."
  (interactive)
  (let ((real-lmt-pt (point-max)))

    ;; If no limit point, default as `point-max'.
    (unless (equal bnd-pt nil)
      (setq real-lmt-pt bnd-pt))

    (forward-char 1)
    (while (and (<= (point) real-lmt-pt)
                (or (jcs-current-char-equal-p " ")
                    (jcs-current-char-equal-p "\t")
                    (jcs-is-beginning-of-line-p)))
      (forward-char 1))))

(defun jcs-first-backward-char-p (c)
  "Check the first character left/backward is C."
  (save-excursion
    ;; NOTE(jenchiech): First fowrad a char and ready to
    ;; be check for next backward character.
    (forward-char 1)
    (jcs-goto-next-backward-char (1+ (jcs-get-beginning-of-line-point)))
    (string= (jcs-get-current-char-string) c)))

(defun jcs-first-forward-char-p (c)
  "Check the first character on the right/forward is C."
  (save-excursion
    (jcs-goto-next-forward-char (jcs-get-end-of-line-point))
    (string= (jcs-get-current-char-string) c)))

(defun jcs-is-there-char-backward-point-p (pt)
  "Check if there is at least one character backward until \
the point.
PT : point."
  (save-excursion
    (jcs-goto-next-backward-char pt)
    (>= (point) pt)))

(defun jcs-is-there-char-forward-point-p (pt)
  "Check if there is at least one character forward until \
the point.
PT : point."
  (save-excursion
    (jcs-goto-next-forward-char pt)
    (<= (point) pt)))

(defun jcs-is-there-char-backward-util-beginning-of-line-p ()
  "Check if there are at least a character on the left until \
the beginning of the line."
  (interactive)
  (jcs-is-there-char-backward-point-p (jcs-get-beginning-of-line-point)))

(defun jcs-is-there-char-forward-until-end-of-line-p ()
  "Check if there are at least a character on the right until \
the end of the line."
  (interactive)
  (jcs-is-there-char-forward-point-p (jcs-get-end-of-line-point)))

;;---------------------------------------------
;; Word
;;---------------------------------------------
(defun jcs-get-word-at-point ()
  "Get word at current cursor position."
  (interactive)
  (thing-at-point 'word))

(defun jcs-current-word-equal-p (str)
  "Check the current word equal to 'STR'."
  (string= (thing-at-point 'word) str))

;;---------------------------------------------
;; Line
;;---------------------------------------------

;;;###autoload
(defun jcs-goto-first-char-in-line ()
  "Goto beginning of line but ignore 'empty characters'(spaces/tabs)."
  (interactive)
  (jcs-back-to-indentation-or-beginning)
  (when (jcs-is-beginning-of-line-p)
    (jcs-back-to-indentation-or-beginning)))

(defun jcs-current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]\t]*$")))

(defun jcs-current-line-totally-empty-p ()
  "Current line empty with no spaces/tabs in there.  (absolute)."
  (and (jcs-is-beginning-of-line-p)
       (jcs-is-end-of-line-p)))

(defun jcs-current-line-comment-p ()
  "Check if current line only comment."
  (save-excursion
    (let ((is-comment-line nil))
      (end-of-line)
      (when (or (jcs-is-inside-comment-block-p)
                (jcs-current-line-empty-p))
        (setq is-comment-line t))
      is-comment-line)))

(defun jcs-get-beginning-of-line-point (&optional ln)
  "Return point at beginning of current line.
LN : line number."
  (save-excursion
    (unless (equal ln nil)
      (goto-line ln))
    (beginning-of-line)
    (point)))

(defun jcs-get-end-of-line-point (&optional ln)
  "Return point at end of current line.
LN : line number."
  (save-excursion
    (unless (equal ln nil)
      (goto-line ln))
    (end-of-line)
    (point)))

(defun jcs-is-end-of-line-p ()
  "Is at the end of line?"
  (= (point) (jcs-get-end-of-line-point)))

(defun jcs-is-end-of-buffer-p ()
  "Is at the end of buffer?"
  (= (point) (point-max)))

(defun jcs-is-beginning-of-line-p ()
  "Is at the beginning of line?"
  (= (current-column) 0))

(defun jcs-is-beginning-of-buffer-p ()
  "Is at the beginning of buffer?"
  (= (point) (point-min)))

(defun jcs-is-current-file-empty-p ()
  "Check if the file a empty file."
  (and (jcs-is-beginning-of-buffer-p)
       (jcs-is-end-of-buffer-p)))

(defun jcs-get-current-line-integer ()
  "Get the current line as integer."
  (string-to-number (jcs-get-current-line-string)))

(defun jcs-get-current-line-string ()
  "Get the current line as string."
  (format-mode-line "%l"))

(defun jcs-is-current-line (line)
  "Is current line number this line?
LINE : number to check if current line this line?"
  (= (string-to-number (format-mode-line "%l")) line))

(defun jcs-is-at-start-of-line-p ()
  "Cursor is at the first character of this line?"
  (let ((current-point nil)
        (firstCharPoint nil))
    (save-excursion
      (setq current-point (point))
      (back-to-indentation)
      (setq firstCharPoint (point)))

    (= firstCharPoint current-point)))

(defun jcs-is-met-first-char-at-line-p ()
  "Check current cursor point is after the first character at \
the current line.

@return { boolean } : true, infront of first character.
false, vice versa."
  (let ((is-infront-of-first-char t)
        (point-to-check nil))
    (save-excursion
      (ignore-errors
        (setq point-to-check (point))
        (beginning-of-line)

        (when (not (jcs-current-line-totally-empty-p))
          (forward-char 1))

        (while (<= (point) point-to-check)
          (if (not (current-whitespacep))
              (setq is-infront-of-first-char nil))
          (forward-char 1))))

    (eq is-infront-of-first-char t)))

(defun jcs-empty-line-between-point (min-pt max-pt)
  "Check if there is empty line between two point.
MIN-PT : smaller position.
MAX-PT : larger position."
  (save-excursion
    (let ((there-is-empty-line nil))
      (when (>= min-pt max-pt)
        (jcs-warning "Min point cannot be larger than max point..")
        ;; Return false.
        (equal there-is-empty-line t))

      (goto-char min-pt)
      (while (< (point) max-pt)
        (when (jcs-current-line-empty-p)
          ;; Return true.
          (setq there-is-empty-line t)
          (equal there-is-empty-line t))
        (jcs-next-line))
      ;; Return false.
      (equal there-is-empty-line t))))

;;;###autoload
(defun jcs-safe-forward-char ()
  "Forward a char if not the end of the line."
  (if (not (jcs-is-beginning-of-line-p))
      (forward-char 1)))

;;;###autoload
(defun jcs-safe-backward-char ()
  "Backward a char if not the beginning of the line."
  (if (not (jcs-is-end-of-line-p))
      (backward-char 1)))


(defun jcs-start-line-in-buffer ()
  "Is current line the start line in buffer."
  (let ((buffer-start-line-num nil)
        ;; Get the current line number in the shell buffer.
        (current-line-num (jcs-get-current-line-integer)))
    ;; Get the last line number in the current shell buffer.
    (save-excursion
      (goto-char (point-min))
      (setq buffer-start-line-num (jcs-get-current-line-integer)))

    ;; Return it.
    (= current-line-num buffer-start-line-num)))

(defun jcs-last-line-in-buffer ()
  "Is current line the last line in buffer."
  (let ((buffer-last-line-num nil)
        ;; Get the current line number in the shell buffer.
        (current-line-num (jcs-get-current-line-integer)))
    ;; Get the last line number in the current shell buffer.
    (save-excursion
      (goto-char (point-max))
      (setq buffer-last-line-num (jcs-get-current-line-integer)))

    ;; Return it.
    (= current-line-num buffer-last-line-num)))

;;---------------------------------------------
;; Move between button.
;;---------------------------------------------
;;URL: https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html

;;;###autoload
(defun jcs-top-most-line ()
  "Move to top of the buffer."
  (interactive)
  ;; NOTE: 0 : top-most-line, -1 : bottom-most-line
  (move-to-window-line-top-bottom 0))

;;;###autoload
(defun jcs-bottom-most-line()
  "Move to bottom of the buffer."
  (interactive)
  ;; NOTE: 0 : top-most-line, -1 : bottom-most-line
  (move-to-window-line-top-bottom -1))

;;---------------------------------------------
;; Mark
;;---------------------------------------------

(defun jcs-is-mark-active ()
  "Is mark active?
@return { boolean } : true, is active. false, is not active."
  (and mark-active
       (= (point) (mark))))

;;---------------------------------------------
;; Region
;;---------------------------------------------

(defun jcs-is-region-selected-p ()
  "Is region active? But if `region-start' and `region-end' is at the same point this would not trigger.
Which normally that mark is active but does not move at all.

@return { boolean } : true, there is region selected. false, no
region selected."
  (use-region-p))

(defun jcs-is-mark-active-or-region-selected-p ()
  "Complete check if the region and the mark is active.

@return { boolean } : true, either region selected or mark is
active. false, there is no region selected and mark is not active.
"
  (or (jcs-is-region-selected-p)
      (jcs-is-mark-active)))

;;;###autoload
(defun jcs-delete-region ()
  "Delete region by default value."
  (interactive)
  (delete-region (region-beginning) (region-end)))

;;---------------------------------------------
;; Comment
;;---------------------------------------------

(defun jcs-is-inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (nth 4 (syntax-ppss)))

;;;###autoload
(defun jcs-goto-start-of-the-comment ()
  "Go to the start of the comment."
  (interactive)
  (when (jcs-is-inside-comment-block-p)
    (backward-char 1)
    (jcs-goto-start-of-the-comment)))

;;;###autoload
(defun jcs-goto-end-of-the-comment ()
  "Go to the end of the comment."
  (interactive)
  (when (jcs-is-inside-comment-block-p)
    (backward-char -1)
    (jcs-goto-end-of-the-comment)))

;;---------------------------------------------
;; Face
;;---------------------------------------------

;;;###autoload
(defun jcs-get-current-point-face ()
  "Get current point's type face as string."
  (interactive)
  (let ((face (or (get-char-property (point) 'font-lock-face)
                  (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    ;; NOTE(jenchieh): this could either be a `string' or a `list'.
    (if (listp face)
        (nth 0 face)
      face)))

;;;###autoload
(defun jcs-what-face ()
  "Print out what face is current cursor on."
  (interactive)
  (message "Current face: %s" current-faces))

(defun jcs-is-current-point-face (in-face)
  "Check if current face the same face as IN-FACE.
Returns, True if is the same as pass in face name string.
False, is not the same as pass in face name string.
IN-FACE : input face name as string."
  (string= in-face (jcs-get-current-point-face)))

(defun jcs-is-default-face-p ()
  "Return non-nil, if is default face.
Return nil, if not default face."
  (or
   ;; STUDY(jenchieh): nil means `default' face, I guess.
   (jcs-is-current-point-face "nil")
   (jcs-is-current-point-face "hl-line")))

;;---------------------------------------------
;; Font
;;---------------------------------------------

;;;###autoload
(defun jcs-list-font-list ()
  "List out all the fonts available."
  (interactive)
  (jcs-log-list (font-family-list)))

;;;###autoload
(defun jcs-change-font (in-font)
  "Choose a font and change that to the current font.
IN-FONT : input font name."
  (interactive
   (list (completing-read
          "Fonts: " (font-family-list))))

  ;; Change the font and keep the size.
  (if (jcs-font-existsp in-font)
      (progn
        (set-frame-font in-font t))
    (progn
      (jcs-error "Font you chose does not exists in current system, Please select other font."))))

(defun jcs-font-existsp (font)
  "Check if font exists?
FONT : font to check."
  (if (string-equal (describe-font font)
                    "No matching font being used")
      nil
    t))

;;---------------------------------------------
;; List
;;---------------------------------------------

(defun jcs-is-in-list-string (in-list str)
  "Check if a string in the string list.
IN-LIST : list of strings.
STR : string to check if is inside the list of strings above."
  (cl-some #'(lambda (lb-sub-str) (string-match lb-sub-str str)) in-list))

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

(defun jcs-get-string-from-file (filePath)
  "Return filePath's file content.
TOPIC(jenchieh): Elisp: Read File Content as String or List of Lines
URL(jenchieh): http://ergoemacs.org/emacs/elisp_read_file_content.html

FILEPATH : file path."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun jcs-project-write-file (in-filename in-content)
  "Write to a file in the project root.
IN-FILENAME : path to write, is relative path to project root.
IN-CONTENT : content/buffer to write to the IN-FILENAME."
  (write-region in-content  ;; Start
                nil  ;; End
                ;; File name (concatenate full path)
                (concat (cdr (project-current))
                        in-filename)  ;; Cache filename.
                ;; Overwrite?
                t))

(defun jcs-parse-ini (filePath)
  "Parse a .ini file.
FILEPATH : .ini file to parse."

  (let ((tmp-ini (jcs-get-string-from-file filePath))
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
  "Return non-nil if the directory/file exists.
Return nil if the directory/file not exists.
FILEPATH : directory/file path."
  (or (file-directory-p filePath)
      (file-exists-p filePath)))

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
    tmp-is-vc-dir))

(defun jcs-up-one-dir-string (dirPath)
  "Go up one directory and return it directory string.
DIRPATH : directory path."
  ;; Remove the last directory in the path.
  (string-match "\\(.*\\)/" dirPath)
  (match-string 1 dirPath))

(defun jcs-vc-root-dir ()
  "Return version control root directory.
If not found, will return empty string."
  (let ((tmp-current-dir (jcs-get-current-dir))
        (tmp-result-dir ""))
    (while (jcs-contain-string "/" tmp-current-dir)
      (when (jcs-is-vc-dir-p tmp-current-dir)
        ;; Return the result, which is the version control path
        ;; or failed to find the version control path.
        (setq tmp-result-dir tmp-current-dir))
      ;; go up one directory.
      (setq tmp-current-dir (jcs-up-one-dir-string tmp-current-dir)))

    (if (string= tmp-result-dir "")
        ""  ;; Not found, return empty string.
      ;; NOTE(jenchieh): if you do not like `/' at the end remove
      ;; concat slash function.
      (concat tmp-result-dir "/"))))

(defun jcs-project-current ()
  "Return the current project's root directory.
Is almost the same as `jcs-vc-root-dir'."
  (cdr (project-current)))

(defun jcs-get-file-name-or-last-dir-fromt-path (in-path &optional ignore-errors-t)
  "Get the either the file name or last directory from the IN-PATH.
IN-PATH : input path.
IGNORE-ERRORS-T : ignore errors for this function?"
  (if (and (not (jcs-file-directory-exists-p in-path))
           (not ignore-errors-t))
      (error "Directory/File you trying get does not exists.")
    (progn
      (let ((result-dir-or-file nil)
            (split-dir-file-list '())
            (split-dir-file-list-len 0))

        (cond ((string-match-p "/" in-path)
               (progn
                 (setq split-dir-file-list (split-string in-path "/"))))
              ((string-match-p "\\" in-path)
               (progn
                 (setq split-dir-file-list (split-string in-path "\\"))))
              ((string-match-p "\\\\" in-path)
               (progn
                 (setq split-dir-file-list (split-string in-path "\\\\")))))

        ;; Get the last element/item in the list.
        (setq split-dir-file-list-len (1- (length split-dir-file-list)))

        ;; Result is alwasy the last item in the list.
        (setq result-dir-or-file (nth split-dir-file-list-len split-dir-file-list))

        ;; Return result.
        result-dir-or-file))))

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

(defun jcs-string-has-no-lowercase (string)
  "Return true iff STRING has no lowercase
SOURCE(jenchieh): https://stackoverflow.com/questions/2129840/check-if-a-string-is-all-caps-in-emacs-lisp"
  (equal (upcase string) string))

(defun jcs-contain-string (in-sub-str in-str)
  "Check if a string is a substring of another string.
Return true if contain, else return false.
IN-SUB-STR : substring to see if contain in the IN-STR.
IN-STR : string to check by the IN-SUB-STR."
  (string-match-p (regexp-quote in-sub-str) in-str))

(defun jcs-is-contain-list-string (in-list in-str)
  "Check if a string contain in any string in the string list.
IN-LIST : list of string use to check if IN-STR in contain one of
the string.
IN-STR : string using to check if is contain one of the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

;;---------------------------------------------
;; Variable
;;---------------------------------------------

(defun jcs-setq-all-local-buffer (in-var in-val)
  "Set all the local buffer to some value.
IN-VAR : input variable name.
IN-VAL : input value to set to IN-VAR."
  (save-window-excursion
    (save-selected-window
      (let ((win-len (length (window-list)))
            (index 0))
        (while (< index win-len)
          (with-current-buffer (buffer-name)
            ;; NOTE(jenchieh): this will actually set whatever the
            ;; variable are. Either global or local variable will work.
            ;;
            ;; TOPIC: Variable references in lisp
            ;; URL: https://stackoverflow.com/questions/1249991/variable-references-in-lisp
            (set in-var (symbol-value in-val)))

          ;; To next window.
          (jcs-other-window-next)
          (setq index (1+ index)))))))

;;---------------------------------------------
;; *Message*
;;---------------------------------------------

;;;###autoload
(defun jcs-erase-message-buffer ()
  "Erase the *Message* buffer."
  (interactive)
  ;; Kill it first.
  (jcs-maybe-kill-this-buffer)

  ;; Message one message to retrieve `*Message*' buffer
  ;; prepare for next use. Or else it some operation
  ;; might prompt some issue that needed `*Message*'
  ;; buffer to be exists.
  (message "Retrieve *Message* buffer.."))
