;;; jcs-util.el --- All utilities put here.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; Buffer

;;;###autoload
(defun jcs-print-current-buffer-name ()
  "Message out what current window's buffer name."
  (interactive)
  (message "Current buffer name: %s" (buffer-name)))

;;;###autoload
(defun jcs-print-current-buffer-file-name ()
  "Message out what current window's buffer file name."
  (interactive)
  (message "Current buffer file name: %s" (buffer-file-name)))

;;;###autoload
(defun jcs-print-current-buffer-status ()
  "Print current buffer related status."
  (interactive)
  (jcs-print-current-buffer-name)
  (jcs-print-current-buffer-file-name))

(defun jcs-buffer-name-or-buffer-file-name ()
  "Sometimes `buffer-file-name` is nil, then return `buffer-name` instead.
Else we just return `buffer-file-name` if available."
  (if (buffer-file-name) (buffer-file-name) (buffer-name)))

(defun jcs-buffer-exists-p (buf-name)
  "Check if the buffer BUF-NAME exists."
  (get-buffer buf-name))

(defun jcs-valid-buffers-in-buffer-list ()
  "See how many valid buffers in the `buffer-list'.
Excluding buffers like `*GNU Emacs*', `*scratch*', etc.
Return number of the valid buffers."
  (let ((cnt 0))
    (dolist (buf (buffer-list))
      (when (buffer-file-name buf)
        (setq cnt (1+ cnt))))
    cnt))

(defun jcs-walk-through-all-buffers-once (fnc)
  "Walk through all the buffers once and execute callback FNC."
  (save-window-excursion
    (dolist (bf (buffer-list))
      (set-buffer bf)
      (when fnc (funcall fnc)))))

(defun jcs-get-buffers (str type)
  "Return a list of buffers that matches STR.
TYPE is the return type; can be 'object or 'string."
  (jcs-get-buffers-regexp (regexp-quote str) type))

(defun jcs-get-buffers-regexp (regexp type)
  "Return a list of buffers that matches REGEXP.
TYPE is the return type; can be 'object or 'string."
  (let ((buf-lst '()))
    (if (not (stringp regexp))
        (user-error "[WARNING] Can't get buffers with this string/regexp: %s" regexp)
      (dolist (buf (buffer-list))
        (setq buf-name (buffer-name buf))
        (when (and (stringp buf-name)
                   (string-match-p regexp buf-name))
          (cl-case type
            ('object (push buf buf-lst))
            ('string (push buf-name buf-lst))))))
    buf-lst))

(defun jcs-do-stuff-if-buffer-exists (buf-or-name fnc)
  "Execute FNC in the BUF-OR-NAME if exists."
  (if (get-buffer buf-or-name)
      (with-current-buffer buf-or-name
        (funcall fnc))
    (message "[WARNING] Can't do stuff with this buffer: %s" buf-or-name)))

(defun jcs-buffer-name-this (name &optional buffer regex)
  "Check if BUFFER's name the same as NAME.
If REGEX is non-nil, check by using regular expression."
  (unless buffer (setq buffer (current-buffer)))
  (if regex
      (string-match-p name (buffer-name buffer))
    (string= name (buffer-name buffer))))

;;----------------------------------------------------------------------------
;; Compile

;;;###autoload
(defun jcs-byte-recompile-directory ()
  "Recompile the current directory."
  (interactive)
  (byte-recompile-directory "./" 0))

;;----------------------------------------------------------------------------
;; Color

(defun jcs-is-hex-code-p (hex-code)
  "Check if the HEX-CODE is valid HEX code."
  (or (string-match-p "#[0-9a-fA-F].." hex-code)
      (string-match-p "#[0-9a-fA-F]....." hex-code)))

(defun jcs--is-light-color-internal (hex-code)
  "Check if the HEX-CODE' light color."
  (let ((is-light nil) (hex-lst '())
        (hex-1 "") (hex-2 "") (hex-3 "")
        ;; 136d = 88h
        (light-central 136) (s2n-base 16))
    ;; Convert symbol to string.
    (when (symbolp hex-code) (setq hex-code (symbol-name hex-code)))
    (if (not (jcs-is-hex-code-p hex-code))
        (message "[WARNING] Hex code to check is invalid: %s" hex-code)
      ;; Remove # from `hex-code'.
      (setq hex-code (jcs-replace-string "#" "" hex-code))
      (setq hex-lst (split-string hex-code ""))
      (setq hex-lst (delete "" hex-lst))
      (if (= (length hex-lst) 6)
          (progn
            (setq hex-1 (concat (nth 0 hex-lst) (nth 1 hex-lst)))
            (setq hex-2 (concat (nth 2 hex-lst) (nth 3 hex-lst)))
            (setq hex-3 (concat (nth 4 hex-lst) (nth 5 hex-lst))))
        (setq hex-1 (nth 0 hex-lst))
        (setq hex-2 (nth 1 hex-lst))
        (setq hex-3 (nth 2 hex-lst)))
      (setq hex-1 (string-to-number hex-1 s2n-base))
      (setq hex-2 (string-to-number hex-2 s2n-base))
      (setq hex-3 (string-to-number hex-3 s2n-base))
      (when (or (> hex-1 light-central)
                (> hex-2 light-central)
                (> hex-3 light-central))
        (setq is-light t)))
    is-light))

(defun jcs-is-light-color (hex-code)
  "Check if the HEX-CODE' light color."
  (and (display-graphic-p)
       (jcs--is-light-color-internal hex-code)))

(defun jcs-is-dark-color (hex-code)
  "Check if the HEX-CODE dark color."
  (and (display-graphic-p)
       (not (jcs-is-light-color hex-code))))

;;----------------------------------------------------------------------------
;; Error

(defun jcs-backtrace-occurs-p ()
  "Check if the backtrace occurs."
  (let ((bb-name "*Backtrace*") (occurs nil))
    (when (get-buffer bb-name)
      (with-current-buffer bb-name
        (setq occurs (not (string-empty-p (buffer-string))))))
    occurs))

;;----------------------------------------------------------------------------
;; Event

(defun jcs-last-input-event-p (te)
  "Check if `last-input-event' a target event, TE."
  (let ((is-event nil))
    (when (listp last-input-event)
      (let ((kn (nth 0 last-input-event)))
        (when (string-match-p te (symbol-name kn))
          (setq is-event t))))
    (when (and (symbolp last-input-event)
               (string= (symbol-name last-input-event) te))
      (setq is-event t))
    is-event))

;;----------------------------------------------------------------------------
;; Excursion Record

(defun jcs--record-window-excursion (fnc)
  "Record the info from an excursion, the FNC and ARGS."
  (save-window-excursion
    (funcall fnc)
    (list (current-buffer) (line-number-at-pos) (current-column))))

(defun jcs--record-window-excursion-apply (record)
  "Apply the RECORD from `jcs--record-window-excursion'."
  (switch-to-buffer-other-window (nth 0 record))
  (jcs-goto-line (nth 1 record))
  (move-to-column (nth 2 record)))

;;----------------------------------------------------------------------------
;; Function

(defun jcs-mute-apply (fnc &rest args)
  "Execute FNC with ARGS without message."
  (let ((message-log-max nil))
    (with-temp-message (or (current-message) nil)
      (let ((inhibit-message t))
        (apply fnc args)))))

;;----------------------------------------------------------------------------
;; Fuzzy

(defun jcs-flx-sort-candidates-by-regex (candidates regex)
  "Sort CANDIDATES by REGEX."
  (require 'flx)
  (let ((scoring-table (make-hash-table))
        (scoring-keys '()))
    (dolist (cand candidates)
      (let* ((scoring (flx-score cand regex))
             ;; Ensure score is not `nil'.
             (score (if scoring (nth 0 scoring) 0)))
        ;; For first time access score with hash-table.
        (unless (gethash score scoring-table) (setf (gethash score scoring-table) '()))
        ;; Push the candidate with the target score to hash-table.
        (push cand (gethash score scoring-table))))
    ;; Get all the keys into a list.
    (maphash (lambda (score-key _cand-lst) (push score-key scoring-keys)) scoring-table)
    (setq scoring-keys (sort scoring-keys #'>))  ; Sort keys in order.
    (setq candidates '())  ; Clean up, and ready for final output.
    (dolist (key scoring-keys)
      (let ((cands (sort (gethash key scoring-table) #'string-lessp)))
        (setq candidates (append candidates cands)))))
  candidates)

;;----------------------------------------------------------------------------
;; Time

(defun jcs-get-timestamp-ver1 ()
  "Get timestamp version 1."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun jcs-get-timestamp-ver2 ()
  "Get timestamp version 2."
  (format-time-string "%Y/%m/%d %H:%M:%S"))

(defun jcs-get-date-ver1 ()
  "Get date buffer in string type - version 1."
  (format-time-string "%Y-%m-%d"))

(defun jcs-get-date-ver2 ()
  "Get date buffer in string type - version 2."
  (format-time-string "%Y/%m/%d"))

(defun jcs-get-year-only ()
  "Get Year buffer in string type."
  (format-time-string "%Y"))

(defun jcs-get-time ()
  "Get time buffer in string type."
  (format-time-string "%H:%M:%S"))

;;;###autoload
(defun jcs-print-timestamps ()
  "Print out all the timestamps."
  (interactive)
  (message "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
  (message "=> Ver. 1 %s" (jcs-get-timestamp-ver1))
  (message "=> Ver. 2 %s" (jcs-get-timestamp-ver2))
  (message "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
  (message "All version of timestamps printed.")
  (jcs-do-after-log-action))

;;----------------------------------------------------------------------------
;; Organize Code

(defun jcs-keep-n-line-between (n-line)
  "Keep N-LINE between the two line of code."
  (save-excursion
    (let ((index 0))
      (while (< index n-line)
        (jcs-keep-one-line-between)
        ;; increament one.
        (setq index (1+ index))))))

;;;###autoload
(defun jcs-keep-one-line-between ()
  "Keep one line between the two line of code."
  (interactive)
  (if (jcs-current-line-empty-p)
      (progn
        (jcs-next-line)
        ;; Kill empty line until there is one line.
        (while (jcs-current-line-empty-p)
          (jcs-kill-whole-line)))
    ;; Make sure have one empty line between.
    (insert "\n")))

;;----------------------------------------------------------------------------
;; Tab / Space

;;;###autoload
(defun jcs-delete-trailing-whitspace-current-line ()
  "Delete the trailing whitespace exist in current line."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((begin-of-line-point nil) (end-of-line-point nil))
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
  "Convert space to tab if current point is space by direction IS-FORWARD."
  (save-excursion
    (let ((good-to-convert nil))
      (save-excursion
        (when (jcs-is-good-space-to-convert-to-tab-p)
          (if is-forward (forward-char 1) (backward-char 1))
          (when (jcs-is-good-space-to-convert-to-tab-p)
            (if is-forward (forward-char 1) (backward-char 1))
            (when (jcs-is-good-space-to-convert-to-tab-p)
              (if is-forward (forward-char 1) (backward-char 1))
              (when (jcs-is-good-space-to-convert-to-tab-p)
                (setq good-to-convert t))))))
      (when good-to-convert
        (if is-forward
            (progn
              (backward-delete-char -1)
              (backward-delete-char -1)
              (backward-delete-char -1)
              (backward-delete-char -1)
              (insert "\t"))
          (backward-delete-char 1)
          (backward-delete-char 1)
          (backward-delete-char 1)
          (backward-delete-char 1)
          (insert "\t"))))))

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
  "Convert tab to space if current point is tab by direction, IS-FORWARD."
  (save-excursion
    (when (jcs-current-char-equal-p "\t")
      (if (equal is-forward t)
          (progn
            (backward-delete-char -1)
            (insert "    "))
        (backward-delete-char 1)
        (insert "    ")))))

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
  (jcs-mute-apply
   (lambda ()
     (save-excursion
       (ignore-errors
         (jcs-goto-first-char-in-line)
         (push-mark-command nil)
         (beginning-of-line)
         (jcs-delete-region)
         (deactivate-mark))))))

;;;###autoload
(defun jcs-insert-spaces-by-tab-width ()
  "Insert spaces depends on tab width configuration."
  (interactive)
  (let* ((tmp-count 0)
         (tab-width (jcs-get-tab-width-record-by-mode))
         (remainder (% (current-column) tab-width))
         (target-width (if (= remainder 0) tab-width (- tab-width remainder))))
    (while (< tmp-count target-width)
      (insert " ")
      (setq tmp-count (1+ tmp-count)))))

;;;###autoload
(defun jcs-backward-delete-spaces-by-tab-width ()
  "Backward delete spaces using tab width."
  (interactive)
  (let* ((tmp-count 0)
         (tab-width (jcs-get-tab-width-record-by-mode))
         (remainder (% (current-column) tab-width))
         (target-width (if (= remainder 0) tab-width remainder)))
    (while (and (< tmp-count target-width)
                (not (jcs-is-beginning-of-line-p))
                (jcs-current-whitespace-p))
      (backward-delete-char 1)
      (setq tmp-count (1+ tmp-count)))))

;;;###autoload
(defun jcs-forward-delete-spaces-by-tab-width ()
  "Forward delete spaces using tab width."
  (interactive)
  (let* ((tmp-count 0)
         (tab-width (jcs-get-tab-width-record-by-mode))
         (remainder (% (jcs-first-char-in-line-column) tab-width))
         (target-width (if (= remainder 0) tab-width remainder)))
    (while (and (< tmp-count target-width)
                (not (jcs-is-end-of-line-p)))
      (let ((is-valid nil))
        (save-excursion
          (forward-char 1)
          (when (jcs-current-whitespace-p)
            (setq is-valid t)))
        (when is-valid
          (backward-delete-char -1)))
      (setq tmp-count (1+ tmp-count)))))

;;----------------------------------------------------------------------------
;; Point

;;;###autoload
(defun jcs-print-current-point ()
  "Print out the current point."
  (interactive)
  (message "Current point: %s" (point)))

(defun jcs-column-to-point (column)
  "Turn the current COLUMN to point."
  (save-excursion
    (move-to-column column)
    (point)))

;;----------------------------------------------------------------------------
;; Character

;;;###autoload
(defun jcs-print-current-char ()
  "Print out the current character."
  (interactive)
  (message "Current character: %s" (string (char-before))))

;; TOPIC: Check if a character (not string) is lowercase,
;; uppercase, alphanumeric?
;; SOURCE: https://stackoverflow.com/questions/27798296/check-if-a-character-not-string-is-lowercase-uppercase-alphanumeric

(defun jcs-word-p (c)
  "Check if C a word."
  (= ?w (char-syntax c)))

(defun jcs-lowercase-p (c)
  "Check if C lowercase."
  (and (jcs-word-p c) (= c (downcase c))))

(defun jcs-uppercase-p (c)
  "Check if C uppercase."
  (and (jcs-word-p c) (= c (upcase c))))

(defun jcs-is-digit-string (c)
  "Check if C is a digit."
  (string-match-p "\^[0-9]'" c))

(defun jcs-current-char-a-wordp ()
  "Check if current character a usual letter."
  (jcs-word-p (string-to-char (jcs-get-current-char-string))))

(defun jcs-current-char-uppercasep ()
  "Check if current character a uppercase character."
  (jcs-uppercase-p (string-to-char (jcs-get-current-char-string))))

(defun jcs-current-char-lowercasep ()
  "Check if current character a lowercase character."
  (not (jcs-current-char-uppercasep)))

(defun jcs-current-whitespace-p ()
  "Check if current character a whitespace character."
  (jcs-current-char-equal-p " "))

(defun jcs-current-tab-p ()
  "Check if current character a tab character."
  (jcs-current-char-equal-p "\t"))

(defun jcs-current-whitespace-or-tab-p ()
  "Check if current character a whitespace or a tab character?"
  (or (jcs-current-char-equal-p " ")
      (jcs-current-char-equal-p "\t")))

(defun jcs-current-char-equal-p (c)
  "Check the current character equal to C, C can be a list of character."
  (cond ((and (stringp c)
              (stringp (jcs-get-current-char-string)))
         (string= (jcs-get-current-char-string) c))
        ((listp c)
         (jcs-is-contain-list-string c (jcs-get-current-char-string)))
        (t nil)))

(defun jcs-current-pos-char-equal-p (c pt)
  "Check PT's character the same as C."
  (save-excursion
    (goto-char pt)
    (jcs-current-char-equal-p c)))

(defun jcs-forward-pos-char-equal-p (c n)
  "Move point N characters forward (backward if N is negative) then check \
the character the same as C."
  (save-excursion
    (forward-char n)
    (jcs-current-char-equal-p c)))

(defun jcs-current-char-string-match-p (c)
  "Check the current character string match to C."
  (if (jcs-is-beginning-of-buffer-p)
      ;; No character at the beginning of the buffer, just return `nil'.
      nil
    (string-match-p c (jcs-get-current-char-string))))

(defun jcs-get-current-char-byte ()
  "Get the current character as the 'byte'."
  (string-to-char (jcs-get-current-char-string)))

(defun jcs-get-current-char-string ()
  "Get the current character as the 'string'."
  (if (char-before)
      (string (char-before))
    ""))

;;;###autoload
(defun jcs-goto-next-backward-char (&optional bnd-pt)
  "Goto the next backward character (not include space/tab).
BND-PT : limit point."
  (interactive)
  (unless bnd-pt (setq bnd-pt (point-min)))
  (unless (jcs-is-beginning-of-buffer-p)
    (forward-char -1)
    (while (and (>= (point) bnd-pt)
                (or (jcs-current-whitespace-or-tab-p)
                    (jcs-is-beginning-of-line-p)))
      (forward-char -1))))

;;;###autoload
(defun jcs-goto-next-forward-char (&optional bnd-pt)
  "Goto the next forward character (not include space/tab).
BND-PT : boundary point."
  (interactive)
  (unless bnd-pt (setq bnd-pt (point-max)))
  (unless (jcs-is-end-of-buffer-p)
    (forward-char 1)
    (while (and (<= (point) bnd-pt)
                (or (jcs-current-whitespace-or-tab-p)
                    (jcs-is-beginning-of-line-p)))
      (forward-char 1))))

(defun jcs-first-backward-char-p (ch)
  "Check the first character on the left/backward is CH or not, limit to the \
whole buffer."
  (save-excursion
    ;; NOTE: First fowrad a char and ready to
    ;; be check for next backward character.
    (forward-char 1)
    (jcs-goto-next-backward-char)
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-first-forward-char-p (ch)
  "Check the first character on the right/forward is CH or not, limit to the \
whole buffer."
  (save-excursion
    (jcs-goto-next-forward-char)
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-first-backward-char-in-line-p (ch)
  "Check the first character on the left/backward is CH or not, with \
current line as boundary."
  (save-excursion
    ;; NOTE: First fowrad a char and ready to
    ;; be check for next backward character.
    (forward-char 1)
    (jcs-goto-next-backward-char (1+ (jcs-get-beginning-of-line-point)))
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-first-forward-char-in-line-p (ch)
  "Check the first character on the right/forward is CH or not with \
current line as boundary."
  (save-excursion
    (jcs-goto-next-forward-char (jcs-get-end-of-line-point))
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-is-there-char-backward-point-p (pt)
  "Check if there is at least one character backward until the point, PT."
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
  (jcs-is-there-char-backward-point-p (jcs-get-beginning-of-line-point)))

(defun jcs-is-there-char-forward-until-end-of-line-p ()
  "Check if there are at least a character on the right until \
the end of the line."
  (jcs-is-there-char-forward-point-p (jcs-get-end-of-line-point)))

;;----------------------------------------------------------------------------
;; Symbol

;;;###autoload
(defun jcs-print-current-symbol ()
  "Print out the current symbol."
  (interactive)
  (message "Current symbol: %s" (jcs-get-symbol-at-point)))

(defun jcs-get-symbol-at-point ()
  "Get symbol at current cursor position."
  (thing-at-point 'symbol))

(defun jcs-form-p-symbol (lst sym val)
  "Form a plist symbol with LST, SYM, VAL."
  (require 'dash)
  (push (plist-put nil sym val) lst)
  (-flatten lst))

(defun jcs-is-start-of-symbol-p ()
  "Check if position end of the symbol."
  (save-excursion
    (let ((cur-pos (point)))
      (forward-symbol 1)
      (forward-symbol -1)
      (= (point) cur-pos))))

(defun jcs-is-end-of-symbol-p ()
  "Check if position end of the symbol."
  (save-excursion
    (let ((cur-pos (point)))
      (forward-symbol -1)
      (forward-symbol 1)
      (= (point) cur-pos))))

;;----------------------------------------------------------------------------
;; Word

;;;###autoload
(defun jcs-print-current-word ()
  "Print out the current word."
  (interactive)
  (message "Current word: %s" (jcs-get-word-at-point)))

(defun jcs-get-word-at-point ()
  "Get word at current cursor position."
  (thing-at-point 'word))

(defun jcs-current-word-equal-p (str)
  "Check the current word equal to 'STR'."
  (string= (thing-at-point 'word) str))

(defun jcs-first-backward-word ()
  "Find out the first backward word from the current cursor position."
  (let ((word ""))
    (save-excursion
      (backward-word 1)
      (setq word (jcs-get-word-at-point)))
    word))

(defun jcs-first-forward-word ()
  "Find out the first backward word from the current cursor position."
  (let ((word ""))
    (save-excursion
      (forward-word 1)
      (setq word (jcs-get-word-at-point)))
    word))

(defun jcs-first-backward-word-p (w)
  "Find out the first backward word from the current cursor position and \
compare W.
Returns non-nil, the word is the same.
Returns nil, the word isn't the same."
  (string= w (jcs-first-backward-word)))

(defun jcs-first-forward-word-p (w)
  "Find out the first forward word from the current cursor position and \
compare W.
Returns non-nil, the word is the same.
Returns nil, the word isn't the same."
  (string= w (jcs-first-forward-word)))

;;----------------------------------------------------------------------------
;; Column

(defun jcs-column-at-pos (&optional pt)
  "Column at PT."
  (unless pt (setq pt (point)))
  (save-excursion
    (goto-char pt)
    (current-column)))

;;----------------------------------------------------------------------------
;; Line

;;;###autoload
(defun jcs-print-current-line ()
  "Print out the current line."
  (interactive)
  (message "Current line: %s" (jcs-get-current-line-string)))

(defun jcs-goto-line (ln)
  "Goto LN line number."
  (goto-char (point-min))
  (forward-line (1- ln)))

;;;###autoload
(defun jcs-goto-first-char-in-line ()
  "Goto beginning of line but ignore 'empty characters'(spaces/tabs)."
  (interactive)
  (jcs-back-to-indentation-or-beginning)
  (when (jcs-is-beginning-of-line-p)
    (jcs-back-to-indentation-or-beginning)))

(defun jcs-first-char-in-line-point ()
  "Return point in first character in line."
  (save-excursion
    (jcs-goto-first-char-in-line)
    (point)))

(defun jcs-first-char-in-line-column ()
  "Return column in first character in line."
  (save-excursion
    (jcs-goto-first-char-in-line)
    (current-column)))

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
  "Return point at beginning of LN."
  (save-excursion
    (when ln (jcs-goto-line ln))
    (beginning-of-line)
    (point)))

(defun jcs-get-end-of-line-point (&optional ln)
  "Return point at end of LN."
  (save-excursion
    (when ln (jcs-goto-line ln))
    (end-of-line)
    (point)))

(defun jcs-is-beginning-of-line-p ()
  "Check if it's at the beginning of line."
  (= (current-column) 0))

(defun jcs-is-beginning-of-buffer-p ()
  "Check if it's at the beginning of buffer."
  (= (point) (point-min)))

(defun jcs-is-end-of-line-p ()
  "Check if it's at the end of line."
  (= (point) (jcs-get-end-of-line-point)))

(defun jcs-is-end-of-buffer-p ()
  "Check if it's at the end of buffer."
  (= (point) (point-max)))

(defun jcs-is-current-file-empty-p (&optional fn)
  "Check if the FN an empty file."
  (if fn
      (with-current-buffer fn
        (and (jcs-is-beginning-of-buffer-p)
             (jcs-is-end-of-buffer-p)))
    (and (jcs-is-beginning-of-buffer-p)
         (jcs-is-end-of-buffer-p))))

(defun jcs-get-current-line-integer ()
  "Get the current line as integer."
  (string-to-number (jcs-get-current-line-string)))

(defun jcs-get-current-line-string ()
  "Get the current line as string."
  (format-mode-line "%l"))

(defun jcs-is-current-line (line)
  "Is current line number this LINE."
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

(defun jcs-is-infront-first-char-at-line-p (&optional pt)
  "Check current cursor PT is before the first character at the current line.
Return non-nil, infront of first character.
Return nil, vice versa."
  (save-excursion
    (let ((is-infront t)
          (point-to-check nil))
      (when pt (goto-char pt))
      (setq point-to-check (point))
      (beginning-of-line)
      (while (and is-infront
                  (< (point) point-to-check)
                  (not (jcs-is-end-of-line-p)))
        (forward-char 1)
        (unless (jcs-current-whitespace-or-tab-p)
          (setq is-infront nil)))
      is-infront)))

(defun jcs-is-behind-last-char-at-line-p (&optional pt)
  "Check current cursor PT is after the last character at the current line.
Return non-nil, behind the last character.
Return nil, vice versa."
  (save-excursion
    (let ((is-behind t))
      (when pt (goto-char pt))
      (while (and is-behind
                  (not (jcs-is-end-of-line-p)))
        (forward-char 1)
        (unless (jcs-current-whitespace-or-tab-p)
          (setq is-behind nil)))
      is-behind)))

(defun jcs-empty-line-between-point (min-pt max-pt)
  "Check if there is empty line between two point, MIN-PT and MAX-PT."
  (save-excursion
    (let ((there-is-empty-line nil))
      (when (>= min-pt max-pt)
        (error "Min point cannot be larger than max point")
        ;; Return false.
        (equal there-is-empty-line t))
      (goto-char min-pt)
      (while (< (point) max-pt)
        (when (jcs-current-line-empty-p)
          ;; Return true.
          (setq there-is-empty-line t)
          there-is-empty-line)
        (jcs-next-line))
      ;; Return false.
      there-is-empty-line)))

(defun jcs-start-line-in-buffer-p ()
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

(defun jcs-last-line-in-buffer-p ()
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

(defun jcs-first-visible-pos-in-window ()
  "First point in current visible window."
  (let ((cp (point)))
    (save-window-excursion
      (save-excursion
        (while (and (not (jcs-is-beginning-of-buffer-p))
                    (pos-visible-in-window-p cp))
          (beginning-of-line)
          (unless (jcs-is-beginning-of-buffer-p)
            (backward-char 1))
          (setq cp (point)))
        (when (and (jcs-is-beginning-of-buffer-p)
                   (pos-visible-in-window-p cp))
          (setq cp (1- (point))))))
    (+ cp 1)))

(defun jcs-last-visible-pos-in-window ()
  "Last point in current visible window."
  (let ((cp (point)))
    (save-window-excursion
      (save-excursion
        (while (and (not (jcs-is-end-of-buffer-p))
                    (pos-visible-in-window-p cp))
          (end-of-line)
          (unless (jcs-is-end-of-buffer-p)
            (forward-char 1))
          (setq cp (point)))))
    (- cp 1)))

(defun jcs-first-visible-line-in-window ()
  "First line number in current visible window."
  (line-number-at-pos (jcs-first-visible-pos-in-window)))

(defun jcs-last-visible-line-in-window ()
  "Last line number in current visible window."
  (line-number-at-pos (jcs-last-visible-pos-in-window)))

(defun jcs-make-first-visible-line-to (ln)
  "Make the first visible line to target line, LN."
  (jcs-goto-line ln)
  (jcs-recenter-top-bottom 'top))

(defun jcs-make-last-visible-line-to (ln)
  "Make the last visible line to target line, LN."
  (jcs-goto-line ln)
  (jcs-recenter-top-bottom 'bottom))

(defun jcs--recenter-positions (type)
  "Return the recenter position value by TYPE."
  (cl-case type (top '(top)) (middle '(middle)) (bottom '(bottom))))

(defun jcs-recenter-top-bottom (type)
  "Recenter the window by TYPE."
  (let ((recenter-positions (jcs--recenter-positions type)))
    (recenter-top-bottom)))

;;----------------------------------------------------------------------------
;; Move between button.

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

;;----------------------------------------------------------------------------
;; Mark

(defun jcs-is-mark-active-p ()
  "Check if the mark active."
  (and mark-active
       (= (point) (mark))))

;;----------------------------------------------------------------------------
;; Region

(defun jcs-is-region-selected-p ()
  "Check if region active.
But if `region-start' and `region-end' is at the same point this would
not trigger.  Which normally that mark is active but does not move at all.

Return non-nil, there is region selected.
Return nil, no region selected."
  (use-region-p))

(defun jcs-is-mark-active-or-region-selected-p ()
  "Complete check if the region and the mark is active.

Return non-nil, either region selected or mark is active.
Return nil, there is no region selected and mark is not active."
  (or (jcs-is-region-selected-p)
      (jcs-is-mark-active-p)))

;;;###autoload
(defun jcs-delete-region ()
  "Delete region by default value."
  (interactive)
  (when (use-region-p) (delete-region (region-beginning) (region-end))))

(defun jcs-select-region (st ed)
  "Select region from ST and ED."
  (goto-char st) (call-interactively #'set-mark-command) (goto-char ed))

;;----------------------------------------------------------------------------
;; Command

(defun jcs-is-command-these-commands (cmd cmds)
  "Check if CMD one of these CMDS.
CMDS should be a list of commands."
  (memq cmd cmds))

;;----------------------------------------------------------------------------
;; Comment

(defun jcs-is-inside-comment-block-p ()
  "Check if current cursor point inside the comment block."
  (nth 4 (syntax-ppss)))

(defun jcs-inside-comment-or-string-p ()
  "Check if inside comment or stirng."
  (or (nth 8 (syntax-ppss))
      (jcs-is-current-point-face "font-lock-comment-face")
      (jcs-is-current-point-face "font-lock-string-face")))

;;;###autoload
(defun jcs-goto-start-of-the-comment ()
  "Go to the start of the comment."
  (interactive)
  (while (jcs-is-inside-comment-block-p)
    (re-search-backward comment-start-skip)))

;;;###autoload
(defun jcs-goto-end-of-the-comment ()
  "Go to the end of the comment."
  (interactive)
  (when (jcs-is-inside-comment-block-p)
    (backward-char -1)
    (jcs-goto-end-of-the-comment)))

;;----------------------------------------------------------------------------
;; Face

;;;###autoload
(defun jcs-print-current-face ()
  "Print out all the faces the current cursor on."
  (interactive)
  (message "Current faces: %s" (jcs-get-current-point-face)))

(defun jcs-get-faces-internal (pos)
  "Return the list of faces at this time."
  (jcs-flatten-list
   (remq nil
         (list
          (get-char-property pos 'read-face-name)
          (get-char-property pos 'face)
          (plist-get (text-properties-at pos) 'face)))))

(defun jcs-get-faces (pos)
  "Get the font faces at POS."
  (require 'flycheck)
  (let ((was-flycheck flycheck-mode)
        (faces nil))
    (setq faces (jcs-get-faces-internal pos))
    (when was-flycheck
      (flycheck-mode -1)
      (setq faces (append faces (jcs-get-faces-internal pos)))
      (flycheck-mode 1))
    faces))

(defun jcs-get-current-point-face (&optional pos)
  "Get current POS's type face as string."
  (unless pos (setq pos (point)))
  (jcs-get-faces pos))

(defun jcs-is-current-point-face (in-face &optional pos)
  "Check if current POS's face the same face as IN-FACE."
  (let ((faces (jcs-get-current-point-face pos)))
    (if (listp faces)
        (if (equal (cl-position in-face faces :test 'string=) nil)
            ;; If return nil, mean not found in the `faces' list.
            nil
          ;; If have position, meaning the face exists.
          t)
      (string= in-face faces))))

(defun jcs-is-default-face-p (&optional pos)
  "Check default face at POS."
  (or (= (length (jcs-get-current-point-face pos)) 0)
      (and (= (length (jcs-get-current-point-face pos)) 1)
           (jcs-is-current-point-face "hl-line"))))

;;----------------------------------------------------------------------------
;; Font

(defun jcs-set-font-size (&optional new-size)
  "Set the font size to NEW-SIZE."
  (unless new-size (setq new-size jcs-default-font-size))
  (set-face-attribute 'default nil :height new-size))

;;;###autoload
(defun jcs-list-font-list ()
  "List out all the fonts available."
  (interactive)
  (jcs-log-list (font-family-list)))

;;;###autoload
(defun jcs-change-font (in-font)
  "Choose a font, IN-FONT and change that to the current font."
  (interactive
   (list (completing-read
          "Fonts: " (font-family-list))))
  ;; Change the font and keep the size.
  (if (jcs-font-existsp in-font)
      (set-frame-font in-font t)
    (error "Font you chose does not exists in current system, please select other font")))

(defun jcs-font-existsp (font)
  "Check if FONT exists."
  (if (string-equal (describe-font font) "No matching font being used")
      nil
    t))

;;;###autoload
(defun jcs-font-lock-fontify-buffer ()
  "Refresh the syntax hightlight for whole buffer."
  (interactive)
  ;; Refresh the syntax hightlight.
  (jcs-mute-apply (lambda () (call-interactively #'font-lock-fontify-buffer))))

;;----------------------------------------------------------------------------
;; List

(defun jcs-flatten-list (l)
  "Flatten the multiple dimensional array, L to one dimensonal array.
For instance,
  '(1 2 3 4 (5 6 7 8)) => '(1 2 3 4 5 6 7 8)."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (jcs-flatten-list a)))))

(defun jcs-remove-nth-element (nth lst)
  "Remove NTH element from the LST and return the list."
  (if (zerop nth)
      (cdr lst)
    (let ((last (nthcdr (1- nth) lst)))
      (setcdr last (cddr last))
      lst)))

(defun jcs-chop (string separator)
  "Split a STRING without consuming a SEPARATOR."
  ;; SOURCE: https://emacs.stackexchange.com/questions/5729/split-a-string-without-consuming-separators
  (cl-loop with seplen = (length separator)
           with len = (length string)
           with start = 0
           with next = seplen
           for end = (or (cl-search separator string :start2 next) len)
           for chunk = (substring string start end)
           collect chunk
           while (< end len)
           do (setf start end next (+ seplen end))))

(defun jcs-concat-string-list (lst-str)
  "Convert list of string, LST-STR to one string."
  (let ((full-str ""))
    (dolist (s lst-str)
      (setq full-str (concat full-str s)))
    full-str))

(defun jcs-is-contain-list-string-regexp (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p lb-sub-str in-str)) in-list))

(defun jcs-is-contain-list-string-regexp-reverse (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST.
The reverse mean the check from regular expression is swapped."
  (cl-some #'(lambda (lb-sub-str) (string-match-p in-str lb-sub-str)) in-list))

(defun jcs-is-contain-list-string (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun jcs-is-contain-list-symbol (in-list in-symbol)
  "Check if IN-SYMBOL contain in any symbol in the IN-LIST."
  (cl-some #'(lambda (lb-sub-symbol) (equal lb-sub-symbol in-symbol)) in-list))

(defun jcs-is-contain-list-integer (in-list in-int)
  "Check if IN-INT contain in any integer in the IN-LIST."
  (cl-some #'(lambda (lb-sub-int) (= lb-sub-int in-int)) in-list))

;;----------------------------------------------------------------------------
;; Minibuffer

(defun jcs-minibuffer-do-stuff (fnc &rest args)
  "Execute FNC and ARGS in minibuffer the safe way."
  (if (not (active-minibuffer-window))
      (user-error "[ERROR] Minibuffer not active to do stuff: %s" fnc)
    (save-selected-window
      (select-window (active-minibuffer-window))
      (apply fnc args))))

;;----------------------------------------------------------------------------
;; Mode

;;;###autoload
(defun jcs-print-current-major-mode ()
  "Print out the current major mode."
  (interactive)
  (message "Current major mode: %s" (symbol-name major-mode)))

(defun jcs-current-major-mode ()
  "Get current major mode."
  major-mode)

(defun jcs-is-current-major-mode-p (mns)
  "Check if this major modes MNS."
  (cond ((stringp mns)
         (string= (symbol-name major-mode) mns))
        ((listp mns)
         (let ((index 0)
               (current-mode-name nil)
               (found nil))
           (while (and (< index (length mns))
                       (not found))
             (setq current-mode-name (nth index mns))
             (setq found (jcs-is-current-major-mode-p current-mode-name))
             (setq index (1+ index)))
           found))
        ((symbolp mns)
         (equal major-mode mns))
        (t nil)))

(defun jcs-is-minor-mode-enabled-p (mode-obj)
  "Check if this minor MODE-OBJ enabled in current buffer/file."
  (bound-and-true-p mode-obj))

(defun jcs-re-enable-mode-if-was-enabled (modename)
  "Re-enable the MODENAME if was enabled."
  (when (symbol-value modename) (jcs-re-enable-mode modename))
  (symbol-value modename))

(defun jcs-re-enable-mode (modename)
  "Re-enable the MODENAME."
  (funcall modename -1)
  (funcall modename 1))

(defun jcs-enable-disable-mode-by-condition (modename predicate)
  "To enable/disable the MODENAME by PREDICATE."
  (if predicate (funcall modename 1) (funcall modename -1)))

;;----------------------------------------------------------------------------
;; I/O

(defun jcs-get-string-from-file (path)
  "Return PATH file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun jcs-project-write-file (in-filename in-content)
  "Write to IN-FILENAME file path relative to the project root with IN-CONTENT content."
  (write-region in-content  ; Start
                nil  ; End
                (concat (cdr (project-current)) in-filename)
                t  ; Overwrite?
                ))

(defun jcs-parse-ini (path)
  "Parse a .ini file by PATH."
  (let ((tmp-ini (jcs-get-string-from-file path))
        (tmp-ini-list '()) (tmp-pair-list nil)
        (tmp-keyword "") (tmp-value "")
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
            (push tmp-keyword tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))
          (let ((tmp-list '()))
            (push tmp-value tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))))
      (setq count (1+ count)))

    ;; return list.
    tmp-ini-list))

(defun jcs-get-properties (ini-list in-key)
  "Get properties data, searched by key and return value.
INI-LIST : ini list.  Please use this with/after using `jcs-parse-ini' function.
IN-KEY : key to search for value."
  (let ((tmp-index 0) (tmp-key "") (tmp-value "") (returns-value ""))
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

;;----------------------------------------------------------------------------
;; File

(defun jcs-get-file-name ()
  "Get current file name."
  (if buffer-file-name
      (file-name-nondirectory buffer-file-name)
    (buffer-name)))

(defun jcs-get-file-name-uppercase ()
  "Get current file name uppercase."
  (upcase (jcs-get-file-name)))

(defun jcs-get-file-name-lowercase ()
  "Get current file name uppercase."
  (downcase (jcs-get-file-name)))

(defun jcs-get-file-name-without-extension ()
  "Get current file name without extension."
  (if buffer-file-name
      (file-name-sans-extension (jcs-get-file-name))
    (buffer-name)))

(defun jcs-get-file-name-without-extension-uppercase ()
  "Get current file name without extension uppercase."
  (upcase (jcs-get-file-name-without-extension)))

(defun jcs-get-file-name-without-extension-lowercase ()
  "Get current file name without extension lowercase."
  (downcase (jcs-get-file-name-without-extension)))

;;----------------------------------------------------------------------------
;; Directory

(defun jcs-get-current-dir ()
  "Return the string of current directory."
  default-directory)

(defun jcs-file-directory-exists-p (file-path)
  "Check if FILE-PATH exists."
  (or (file-directory-p file-path)
      (file-exists-p file-path)))

(defun jcs-is-vc-dir-p (dir-path)
  "Check if DIR-PATH a version control directory."
  (let ((tmp-is-vc-dir nil))
    (dolist (tmp-vc-type grep-find-ignored-directories)
      (let ((tmp-check-dir (concat dir-path "/" tmp-vc-type)))
        (when (jcs-file-directory-exists-p tmp-check-dir)
          (setq tmp-is-vc-dir t))))
    ;; Return retult.
    tmp-is-vc-dir))

(defun jcs-up-one-dir-string (dir-path)
  "Go up one directory from DIR-PATH and return it directory string."
  (string-match "\\(.*\\)/" dir-path)  ; Remove the last directory in the path.
  (match-string 1 dir-path))

(defun jcs-vc-root-dir ()
  "Check if version control directory."
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
      ;; NOTE: if you do not like `/' at the end remove
      ;; concat slash function.
      (concat tmp-result-dir "/"))))

(defun jcs-project-current ()
  "Return project directory path."
  (cdr (project-current)))

(defun jcs-get-file-name-or-last-dir-fromt-path (in-path &optional ignore-errors-t)
  "Get the either the file name or last directory from the IN-PATH.
IN-PATH : input path.
IGNORE-ERRORS-T : ignore errors for this function?"
  (if (and (not (jcs-file-directory-exists-p in-path))
           (not ignore-errors-t))
      (error "Directory/File you trying get does not exists")
    (let ((result-dir-or-file nil)
          (split-dir-file-list '())
          (split-dir-file-list-len 0))

      (cond ((string-match-p "/" in-path)
             (setq split-dir-file-list (split-string in-path "/")))
            ((string-match-p "\\" in-path)
             (setq split-dir-file-list (split-string in-path "\\")))
            ((string-match-p "\\\\" in-path)
             (setq split-dir-file-list (split-string in-path "\\\\"))))

      ;; Get the last element/item in the list.
      (setq split-dir-file-list-len (1- (length split-dir-file-list)))

      ;; Result is alwasy the last item in the list.
      (setq result-dir-or-file (nth split-dir-file-list-len split-dir-file-list))

      ;; Return result.
      result-dir-or-file)))

;;----------------------------------------------------------------------------
;; String

;;;###autoload
(defun jcs-print-current-string ()
  "Print out the current string at current point."
  (interactive)
  (message "Current string: %s" (jcs-string-at-point)))

(defun jcs-fill-n-char-seq (ch-seq n)
  "Fill CH-SEQ with N length."
  (let ((ch-out ch-seq))
    (if (not (stringp ch-out))
        (setq ch-out nil)
      (unless (or (numberp n) (jcs-is-positive n)) (setq n 1))
      (while (< (length ch-out) n)
        (setq ch-out (concat ch-out ch-seq))))
    (if ch-out (substring ch-out 0 n) nil)))

(defun jcs-is-inside-string-p ()
  "Check if current cursor point inside the string."
  (nth 3 (syntax-ppss)))

;;;###autoload
(defun jcs-goto-start-of-the-string ()
  "Go to the start of the string."
  (interactive)
  (when (jcs-is-inside-string-p)
    (backward-char 1)
    (jcs-goto-start-of-the-string)))

;;;###autoload
(defun jcs-goto-end-of-the-string ()
  "Go to the start of the string."
  (interactive)
  (when (jcs-is-inside-string-p)
    (forward-char 1)
    (jcs-goto-end-of-the-string)))

(defun jcs-string-at-point (&optional pt)
  "Get the string at PT."
  (save-excursion
    (when pt
      (goto-char pt))
    (let ((ret-str nil) (st-str -1) (ed-str -1))
      (save-excursion
        (jcs-goto-start-of-the-string)
        (setq st-str (point)))
      (save-excursion
        (jcs-goto-end-of-the-string)
        (setq ed-str (point)))
      (unless (= st-str ed-str)
        (setq ret-str (buffer-substring-no-properties (1+ st-str) (1- ed-str))))
      ret-str)))

(defun jcs-remove-string-by-substring (str substr)
  "Remove a STR by a SUBSTR."
  (s-replace substr "" str))

(defun jcs-replace-string (rp-tar rp-str str)
  "Replace a RP-TAR with RP-STR in STR."
  (require 's)
  (s-replace rp-tar rp-str str))

(defun jcs-parse-bool (in-str)
  "Parse IN-STR to boolean type value."
  (let ((tmp-bool nil))
    (when (or (string= in-str "True")
              (string= in-str "true")
              (string= in-str "t"))
      (setq tmp-bool t))
    ;; return result.
    tmp-bool))

(defun jcs-string-has-no-lowercase (str)
  "Return non-nil, if STR has no lowercase."
  ;; SOURCE: https://stackoverflow.com/questions/2129840/check-if-a-string-is-all-caps-in-emacs-lisp
  (equal (upcase str) str))

(defun jcs-contain-string (in-sub-str in-str)
  "Check if the IN-SUB-STR is a string in IN-STR."
  (string-match-p (regexp-quote in-sub-str) in-str))

(defun jcs-last-char-in-string (reg str)
  "Find the position in STR using REG from th end."
  (let ((pos -1)
        (run-it t))
    (while run-it
      (setq run-it (string-match-p reg str (1+ pos)))
      (when run-it (setq pos run-it)))
    (if (= pos -1) nil pos)))

;;----------------------------------------------------------------------------
;; Variable

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
            ;; NOTE: this will actually set whatever the
            ;; variable are. Either global or local variable will work.
            ;;
            ;; TOPIC: Variable references in lisp
            ;; URL: https://stackoverflow.com/questions/1249991/variable-references-in-lisp
            (set in-var (symbol-value in-val)))

          ;; To next window.
          (jcs-other-window-next)
          (setq index (1+ index)))))))

;;----------------------------------------------------------------------------
;; Loop

(defun jcs-loop-times (fnc cnt &optional st)
  "Do FNC with CNT from ST."
  (unless st (setq st 0))
  (let ((index st))
    (while (< index cnt)
      (funcall fnc index)
      (setq index (1+ index)))))

;;----------------------------------------------------------------------------
;; Loading

(defun jcs-with-eval-after-load-multiple (files &rest body)
  "Execute BODY after one of the FILES is loaded."
  (dolist (file files) (with-eval-after-load file (dolist (bd body) (funcall bd)))))

;;----------------------------------------------------------------------------
;; System

;;;###autoload
(defun jcs-print-current-system ()
  "Print out the current system info."
  (interactive)
  (message "Current system: %s - %s"
           (jcs-get-current-sysem)
           (system-name)))

(defun jcs-get-current-sysem ()
  "Return the current operating system."
  (cond (jcs-is-windows 'dos)
        (jcs-is-bsd 'mac)
        (jcs-is-linux 'unix)
        (t nil)))

;;;###autoload
(defun jcs-ask-line-endings-for-this-sh-script (type)
  "Ask the saved line endings TYPE for this shell script."
  (require 'show-eol)
  (interactive
   (list
    (completing-read
     "Line Endings Type: "
     (let ((read-lst '("Windows (dos)" "macOS (mac)" "Linux (unix)")))
       (push (format "=> system: (%s)" (jcs-get-current-sysem)) read-lst)
       (push (format "=> file: (%s)" (show-eol--get-current-system)) read-lst)
       read-lst))))
  (let ((sys-type nil))
    (cond ((string= type "Windows (dos)") (setq sys-type 'dos))
          ((string= type "macOS (mac)") (setq sys-type 'mac))
          ((string= type "Linux (unix)") (setq sys-type 'unix))
          ((string-match-p "file" type) (setq sys-type (show-eol--get-current-system)))
          ((string-match-p "system" type) (setq sys-type (jcs-get-current-sysem))))
    (set-buffer-file-coding-system sys-type)))

;;----------------------------------------------------------------------------
;; Parentheses

(defun jcs-find-pair-paren (beg-ch end-ch direction)
  "Find pair parenthese with BEG-CH, END-CH and DIRECTION."
  (let ((beg-cnt 0) (end-cnt 0) (fnc nil) (lim-pt -1))
    (cl-case direction
      ('backward
       (setq lim-pt (point-min))
       (setq end-cnt 1)
       (setq fnc 'backward-char))
      ('forward
       (setq lim-pt (point-max))
       (setq beg-cnt 1)
       (setq fnc 'forward-char)))
    (if (not fnc)
        (user-error "Can't find pair parenthese with this '%s' define" direction)
      (while (and (not (= beg-cnt end-cnt))
                  (not (= (point) lim-pt)))
        (funcall fnc 1)
        (cond ((jcs-current-char-equal-p beg-ch) (setq beg-cnt (1+ beg-cnt)))
              ((jcs-current-char-equal-p end-ch) (setq end-cnt (1+ end-cnt))))))))


(provide 'jcs-util)
;;; jcs-util.el ends here
