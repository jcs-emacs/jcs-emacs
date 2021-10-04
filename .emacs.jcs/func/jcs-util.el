;;; jcs-util.el --- All utilities put here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Macro" )
;;

(defmacro jcs-with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now")) (body `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.3fs)" ,title elapsed))))))

(defmacro jcs-save-excursion (&rest body)
  "Re-implementation `save-excursion' in FNC with ARGS."
  (declare (indent 0) (debug t))
  `(let ((ln (line-number-at-pos nil t)) (col (current-column)))
     (progn ,@body)
     (jcs-goto-line ln)
     (move-to-column col)))

(defmacro jcs-point-at-pos (&rest body)
  "Execute BODY when return point."
  (declare (indent 0) (debug t))
  `(save-excursion (progn ,@body) (point)))

(defmacro jcs-save-scroll-conservatively (&rest body)
  "Execute BODY by saving value of variable `scroll-conservatively'."
  (declare (indent 0) (debug t))
  `(progn
     (jcs-scroll-conservatively-disable)
     (progn ,@body)
     (redisplay)
     (jcs-scroll-conservatively-enable)))

(defmacro jcs-save-window-excursion (&rest body)
  "Execute BODY without touching window's layout/settings."
  (declare (indent 0) (debug t))
  `(progn
     (jcs-window-record-once)
     (progn ,@body)
     (jcs-window-restore-once)))

(defmacro jcs-try-run (repetitions &rest body)
  "Try execute BODY with REPETITIONS of times."
  (declare (indent 1) (debug t))
  `(let ((cnt 0) break)
     (while (and (null break) (not (ignore-errors (progn ,@body))))
       (setq cnt (1+ cnt)
             break (<= ,repetitions cnt)))))

;;
;; (@* "Advice" )
;;

(defun jcs-key-advice-add (key where fnc)
  "Safe add advice KEY with FNC at WHERE."
  (let ((key-fnc (key-binding (kbd key))))
    (when (symbolp key-fnc) (advice-add key-fnc where fnc))))

(defun jcs-key-advice-remove (key fnc)
  "Safe remove advice KEY with FNC."
  (let ((key-fnc (key-binding (kbd key))))
    (when (symbolp key-fnc) (advice-remove key-fnc fnc))))

;;
;; (@* "Association List" )
;;

(defun jcs-set-alist (alist key value)
  "Update ALIST with KEY and VALUE."
  (if (assoc key alist)
      (setf (cdr (assoc key alist)) value)
    (push (cons key value) alist))
  alist)

;;
;; (@* "Buffer" )
;;

(defun jcs-print-current-buffer-info ()
  "Message out what current window's buffer name."
  (interactive)
  (message "[INFO] Name: %s" (buffer-name))
  (message "[INFO] Filename: %s" (buffer-file-name)))

(defun jcs-buffer-name-or-buffer-file-name (&optional buf)
  "Return BUF's `buffer-file-name' or `buffer-name' respectively."
  (or (buffer-file-name buf) (buffer-name buf)))

(defun jcs-buffer-exists-p (buf-name)
  "Return non-nil if the buffer BUF-NAME exists."
  (get-buffer buf-name))

(defun jcs-virtual-buffer-p (&optional buffer)
  "Return non-nil if buffer doesn't exists on disk."
  (not (jcs-valid-buffer-p buffer)))

(defun jcs-valid-buffer-p (&optional buffer)
  "Return non-nil if buffer does exists on disk."
  (let ((bfn (buffer-file-name buffer)))
    (and bfn (file-exists-p bfn))))

(defun jcs-invalid-buffer-p (&optional buffer)
  "Return non-nil if buffer does't exists on disk but has a valid file path.
This occurs when file was opened but has moved to somewhere else externally."
  (let ((bfn (buffer-file-name buffer)))
    (and bfn (not (file-exists-p bfn)))))

(defun jcs-virtual-buffer-list ()
  "Return a list of virtual buffers."
  (let ((buf-lst (buffer-list)) lst)
    (dolist (buf buf-lst) (when (jcs-virtual-buffer-p buf) (push buf lst)))
    (reverse lst)))

(defun jcs-valid-buffer-list ()
  "Return a list of valid buffers."
  (let ((buf-lst (buffer-list)) (lst '()))
    (dolist (buf buf-lst) (when (jcs-valid-buffer-p buf) (push buf lst)))
    (reverse lst)))

(defun jcs-invalid-buffer-list ()
  "Return a list of invalid buffers."
  (let ((buf-lst (buffer-list)) (lst '()))
    (dolist (buf buf-lst) (when (jcs-invalid-buffer-p buf) (push buf lst)))
    (reverse lst)))

(defun jcs-valid-buffers-count ()
  "Return number of the valid buffers."
  (length (jcs-valid-buffer-list)))

(defun jcs-invalid-buffers-count ()
  "Return number of the invalid buffers."
  (length (jcs-invalid-buffer-list)))

(defun jcs-valid-buffers-exists-p ()
  "Check to see if any valid buffer exists in buffer list."
  (> (jcs-valid-buffers-count) 0))

(defun jcs-walk-through-all-buffers-once (fnc)
  "Walk through all the buffers once and execute callback FNC."
  (save-window-excursion
    (dolist (bf (buffer-list)) (set-buffer bf) (when fnc (funcall fnc)))))

(defun jcs-get-buffers (str type)
  "Return a list of buffers that match STR.
TYPE is the return type; can be 'object or 'string."
  (jcs-get-buffers-regexp (regexp-quote str) type))

(defun jcs-get-buffers-regexp (regexp type)
  "Return a list of buffers that match REGEXP.
TYPE is the return type; can be 'object or 'string."
  (let (buf-lst buf-name)
    (if (not (stringp regexp))
        (user-error "[WARNING] Can't get buffers with this string/regexp: %s" regexp)
      (dolist (buf (buffer-list))
        (setq buf-name (buffer-name buf))
        (when (and (stringp buf-name) (string-match-p regexp buf-name))
          (cl-case type
            (object (push buf buf-lst))
            (string (push buf-name buf-lst))))))
    buf-lst))

(defun jcs-do-stuff-if-buffer-exists (buf-or-name fnc)
  "Execute FNC in the BUF-OR-NAME if exists."
  (if (get-buffer buf-or-name) (with-current-buffer buf-or-name (funcall fnc))
    (message "[WARNING] Can't do stuff with this buffer: %s" buf-or-name)))

(defun jcs-buffer-name-this (name &optional buffer regex)
  "Return non-nil if BUFFER's name is the same as NAME.

If optional argument is non-nil, check by using regular expression instead."
  (unless buffer (setq buffer (current-buffer)))
  (if regex (string-match-p name (buffer-name buffer))
    (string= name (buffer-name buffer))))

(defun jcs-get-buffer-by-path (path)
  "Return the buffer by file PATH.

Notice PATH can either be `buffer-name' or `buffer-file-name'."
  (let ((buf-lst (buffer-list)) target-buf)
    (when (cl-some (lambda (buf)
                     (setq target-buf buf)
                     (string= (jcs-buffer-name-or-buffer-file-name buf) path))
                   buf-lst)
      target-buf)))

(defun jcs-buffer-filter (name &optional type)
  "Return a list of buffers with NAME.

See function `jcs-string-compare-p' for argument TYPE."
  (let (lst)
    (dolist (buf (buffer-list))
      (when (jcs-string-compare-p name (buffer-name buf) type)
        (push buf lst)))
    lst))

;;
;; (@* "Compile" )
;;

(defun jcs-byte-recompile-directory ()
  "Recompile the current directory."
  (interactive)
  (byte-recompile-directory "./" 0))

;;
;; (@* "Color" )
;;

(defun jcs-is-hex-code-p (hex-code)
  "Check if the HEX-CODE is valid HEX code."
  (or (string-match-p "#[0-9a-fA-F].." hex-code)
      (string-match-p "#[0-9a-fA-F]....." hex-code)))

(defun jcs--is-light-color-internal (hex-code)
  "Check if the HEX-CODE' light color."
  (let ((is-light nil) (hex-lst '()) (hex-1 "") (hex-2 "") (hex-3 "")
        ;; 136d = 88h
        (light-central 136) (s2n-base 16))
    ;; Convert symbol to string.
    (when (symbolp hex-code) (setq hex-code (symbol-name hex-code)))
    (if (not (jcs-is-hex-code-p hex-code))
        (user-error "[WARNING] Hex code to check is invalid: %s" hex-code)
      ;; Remove # from `hex-code'.
      (setq hex-code (jcs-replace-string "#" "" hex-code)
            hex-lst (split-string hex-code "")
            hex-lst (delete "" hex-lst))
      (if (= (length hex-lst) 6)
          (setq hex-1 (concat (nth 0 hex-lst) (nth 1 hex-lst))
                hex-2 (concat (nth 2 hex-lst) (nth 3 hex-lst))
                hex-3 (concat (nth 4 hex-lst) (nth 5 hex-lst)))
        (setq hex-1 (nth 0 hex-lst)
              hex-2 (nth 1 hex-lst)
              hex-3 (nth 2 hex-lst)))
      (setq hex-1 (string-to-number hex-1 s2n-base)
            hex-2 (string-to-number hex-2 s2n-base)
            hex-3 (string-to-number hex-3 s2n-base))
      (when (or (> hex-1 light-central) (> hex-2 light-central)
                (> hex-3 light-central))
        (setq is-light t)))
    is-light))

(defun jcs-is-light-color-p (hex-code)
  "Check if the HEX-CODE' light color."
  (and (display-graphic-p) (jcs--is-light-color-internal hex-code)))

(defun jcs-is-dark-color-p (hex-code)
  "Check if the HEX-CODE dark color."
  (and (display-graphic-p) (not (jcs-is-light-color-p hex-code))))

;;
;; (@* "Error" )
;;

(defun jcs-backtrace-occurs-p ()
  "Check if the backtrace occurs."
  (let ((bb-name "*Backtrace*") occurs)
    (when (get-buffer bb-name)
      (with-current-buffer bb-name
        (setq occurs (not (string-empty-p (buffer-string))))))
    occurs))

;;
;; (@* "Event" )
;;

(defun jcs-last-input-event-p (te)
  "Check if `last-input-event' a target event, TE."
  (let (is-event)
    (when (listp last-input-event)
      (let ((kn (nth 0 last-input-event)))
        (when (string-match-p te (symbol-name kn))
          (setq is-event t))))
    (when (and (symbolp last-input-event)
               (string= (symbol-name last-input-event) te))
      (setq is-event t))
    is-event))

;;
;; (@* "Excursion Record" )
;;

(defun jcs--record-window-excursion (fnc)
  "Record the info from an excursion, the FNC and ARGS."
  (save-excursion
    (save-window-excursion
      (let ((success (ignore-errors (funcall fnc))))
        (when success
          (with-current-buffer (if (bufferp success) success (current-buffer))
            (list (current-buffer) (line-number-at-pos) (current-column)
                  (jcs-first-visible-line-in-window))))))))

(defun jcs--record-window-excursion-apply (record)
  "Apply the RECORD from `jcs--record-window-excursion'."
  (if (not record)
      (user-error "[INFO] No definition found for current target")
    (jcs-switch-to-next-window-larger-in-height)
    (switch-to-buffer (nth 0 record))
    (jcs-make-first-visible-line-to (nth 3 record))
    (jcs-goto-line (nth 1 record))
    (move-to-column (nth 2 record))))

;;
;; (@* "Function" )
;;

(defmacro jcs-unmute-apply (&rest body)
  "Execute BODY with ensuring message log."
  (declare (indent 0) (debug t))
  `(let ((message-log-max jcs-message-log-max)) (progn ,@body)))

(defmacro jcs-mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let ((message-log-max nil))
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) (progn ,@body)))))

(defmacro jcs-no-log-apply (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let ((message-log-max nil)) (progn ,@body)))

(defun jcs-funcall-fboundp (fnc &rest args)
  "Call FNC with ARGS if exists."
  (when (fboundp fnc) (if args (funcall fnc args) (funcall fnc))))

;;
;; (@* "Fuzzy" )
;;

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

;;
;; (@* "Key" )
;;

(defun jcs-print-current-keymap ()
  "Message out what current keymap."
  (interactive)
  (message "[INFO] Current keymap: %s" (jcs-current-keymap)))

(defun jcs-keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

(defun jcs-current-keymap ()
  "Return SYMBOL represent the keymap."
  (or (jcs-keymap-symbol (current-local-map))
      ;; We try to guess the possible keymap by their major-mode name
      (intern (concat (symbol-name major-mode) "-map"))))

(defun jcs-bind-key (key def &optional keymap)
  "Like `define-key' but default to current KEYMAP.

See description from function `define-key' for arguments KEY, DEF and KEYMAP."
  (let* ((mode-map (symbol-value (jcs-current-keymap)))
         (keymap (or keymap mode-map)))
    (if (keymapp keymap) (define-key keymap key def)
      (user-error "[WARNING] Failed to bind key `%s`, `%s`, `%s`" keymap key def))))

;;
;; (@* "Time" )
;;

(defun jcs-timestamp-ver1 ()
  "Get timestamp version 1."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun jcs-timestamp-ver2 ()
  "Get timestamp version 2."
  (format-time-string "%Y/%m/%d %H:%M:%S"))

(defun jcs-get-date-ver1 ()
  "Get date buffer in string type - version 1."
  (format-time-string "%Y-%m-%d"))

(defun jcs-get-date-ver2 ()
  "Get date buffer in string type - version 2."
  (format-time-string "%Y/%m/%d"))

(defun jcs-month-name ()
  "Get Month name in string type."
  (format-time-string "%B"))

(defun jcs-month-name-abbrev ()
  "Get Month name abbreviation in string type."
  (format-time-string "%b"))

(defun jcs-day-only ()
  "Get Day in string type."
  (format-time-string "%d"))

(defun jcs-month-only ()
  "Get Month in string type."
  (format-time-string "%m"))

(defun jcs-year-only ()
  "Get Year in string type."
  (format-time-string "%Y"))

(defun jcs-get-time ()
  "Get time buffer in string type."
  (format-time-string "%H:%M:%S"))

(defun jcs-print-timestamps ()
  "Print out all the timestamps."
  (interactive)
  (message (jcs-env-separator))
  (message "=> Ver. 1 %s" (jcs-timestamp-ver1))
  (message "=> Ver. 2 %s" (jcs-timestamp-ver2))
  (message (jcs-env-separator))
  (message "[INFO] All version of timestamps printed.")
  (jcs-do-after-log-action))

;;
;; (@* "Timer" )
;;

(defun jcs-safe-kill-timer (tmr)
  "Kill timer (TMR) the safe way."
  (when (timerp tmr) (cancel-timer tmr) (setf tmr nil) tmr))

;;
;; (@* "Organize Code" )
;;

(defun jcs-keep-n-line-between (n-line)
  "Keep N-LINE between the two line of code."
  (save-excursion
    (let ((index 0))
      (while (< index n-line)
        (jcs-keep-one-line-between)
        ;; increament one.
        (setq index (1+ index))))))

(defun jcs-keep-one-line-between ()
  "Keep one line between the two line of code."
  (interactive)
  (if (jcs-current-line-empty-p)
      (progn
        (jcs-next-line)
        ;; Kill empty line until there is one line.
        (while (jcs-current-line-empty-p) (jcs-kill-whole-line)))
    ;; Make sure have one empty line between.
    (insert "\n")))

;;
;; (@* "Tab / Space" )
;;

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
        (if is-forward (backward-delete-char -4) (backward-delete-char 4))
        (insert "\t")))))

(defun jcs-backward-convert-space-to-tab ()
  "Convert space to tab backward at point."
  (interactive)
  (jcs-convert-space-to-tab nil))

(defun jcs-forward-convert-space-to-tab ()
  "Convert space to tab forward at point."
  (interactive)
  (jcs-convert-space-to-tab t))

(defun jcs-convert-tab-to-space (is-forward)
  "Convert tab to space if current point is tab by direction, IS-FORWARD."
  (save-excursion
    (when (jcs-current-char-equal-p "\t")
      (if is-forward (backward-delete-char -1) (backward-delete-char 1))
      (insert "    "))))

(defun jcs-backward-convert-tab-to-space ()
  "Convert tab to space backward at point."
  (interactive)
  (jcs-convert-tab-to-space nil))

(defun jcs-forward-convert-tab-to-space ()
  "Convert tab to space forward at point."
  (interactive)
  (jcs-convert-tab-to-space t))

(defun jcs-delete-space-infront-of-line ()
  "Delete tab/spaces before the first character in line."
  (interactive)
  (jcs-mute-apply
    (save-excursion
      (ignore-errors
        (jcs-goto-first-char-in-line)
        (push-mark-command nil)
        (beginning-of-line)
        (jcs-delete-region)
        (deactivate-mark)))))

;;
;; (@* "Indentation" )
;;

(defun jcs-insert-spaces-by-indent-level ()
  "Insert spaces depends on indentation level configuration."
  (interactive)
  (let* ((tmp-count 0)
         (indent-lvl (indent-control-get-indent-level-by-mode))
         (remainder (% (current-column) indent-lvl))
         (target-width (if (= remainder 0) indent-lvl (- indent-lvl remainder))))
    (while (< tmp-count target-width)
      (insert " ")
      (setq tmp-count (1+ tmp-count)))))

(defun jcs-backward-delete-spaces-by-indent-level ()
  "Backward delete spaces using indentation level."
  (interactive)
  (let* ((tmp-count 0)
         (indent-lvl (indent-control-get-indent-level-by-mode))
         (remainder (% (current-column) indent-lvl))
         (target-width (if (= remainder 0) indent-lvl remainder))
         success)
    (while (and (< tmp-count target-width)
                (not (jcs-is-beginning-of-line-p))
                (jcs-current-whitespace-p))
      (backward-delete-char 1)
      (setq success t
            tmp-count (1+ tmp-count)))
    success))

(defun jcs-forward-delete-spaces-by-indent-level ()
  "Forward delete spaces using indentation level."
  (interactive)
  (let* ((tmp-count 0)
         (indent-lvl (indent-control-get-indent-level-by-mode))
         (remainder (% (jcs-first-char-in-line-column) indent-lvl))
         (target-width (if (= remainder 0) indent-lvl remainder))
         success)
    (while (and (< tmp-count target-width) (not (jcs-is-end-of-line-p)))
      (let ((is-valid nil))
        (save-excursion
          (forward-char 1)
          (when (jcs-current-whitespace-p) (setq is-valid t)))
        (when is-valid (backward-delete-char -1) (setq success t)))
      (setq tmp-count (1+ tmp-count)))
    success))

;;
;; (@* "Point" )
;;

(defun jcs-print-current-point ()
  "Print out the current point."
  (interactive)
  (message "[INFO] Current point: %s" (point)))

(defun jcs-column-to-point (column)
  "Turn the current COLUMN to point."
  (save-excursion
    (move-to-column column)
    (point)))

;;
;; (@* "Character" )
;;

(defun jcs-print-current-char ()
  "Print out the current character."
  (interactive)
  (message "[INFO] Current character: %s" (jcs-get-current-char-string)))

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
         (jcs-contain-list-string c (jcs-get-current-char-string)))
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
    (jcs-safe-forward-char n)
    (jcs-current-char-equal-p c)))

(defun jcs-current-char-string-match-p (c)
  "Check the current character string match to C."
  (if (bobp)
      ;; No character at the beginning of the buffer, just return `nil'.
      nil
    (string-match-p c (jcs-get-current-char-string))))

(defun jcs-get-current-char-byte ()
  "Get the current character as the 'byte'."
  (string-to-char (jcs-get-current-char-string)))

(defun jcs-get-current-char-string ()
  "Get the current character as the 'string'."
  (if (char-before) (string (char-before)) ""))

(defun jcs-goto-next-backward-char (&optional bnd-pt)
  "Goto the next backward character (not include space/tab).
BND-PT : limit point."
  (interactive)
  (unless bnd-pt (setq bnd-pt (point-min)))
  (unless (bobp)
    (forward-char -1)
    (while (and (>= (point) bnd-pt)
                (or (jcs-current-whitespace-or-tab-p) (jcs-is-beginning-of-line-p)))
      (forward-char -1))))

(defun jcs-goto-next-forward-char (&optional bnd-pt)
  "Goto the next forward character (not include space/tab).
BND-PT : boundary point."
  (interactive)
  (unless bnd-pt (setq bnd-pt (point-max)))
  (unless (jcs-is-end-of-buffer-p)
    (forward-char 1)
    (while (and (<= (point) bnd-pt)
                (or (jcs-current-whitespace-or-tab-p) (jcs-is-beginning-of-line-p)))
      (forward-char 1))))

(defun jcs-first-backward-char-p (ch)
  "Check the first character on the left is CH or not, limit to the whole buffer."
  (save-excursion
    ;; NOTE: First fowrad a char and ready to be check for next backward character.
    (forward-char 1)
    (jcs-goto-next-backward-char)
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-first-forward-char-p (ch)
  "Check the first character on the right is CH or not, limit to the whole buffer."
  (save-excursion
    (jcs-goto-next-forward-char)
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-first-backward-char-in-line-p (ch)
  "Check the first character on the left is CH or not, with current line as boundary."
  (save-excursion
    ;; NOTE: First fowrad a char and ready to be check for next backward character.
    (forward-char 1)
    (jcs-goto-next-backward-char (1+ (jcs-get-beginning-of-line-point)))
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-first-forward-char-in-line-p (ch)
  "Check the first character on the right is CH or not with current line as boundary."
  (save-excursion
    (jcs-goto-next-forward-char (jcs-get-end-of-line-point))
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-is-there-char-backward-point-p (pt)
  "Check if there is at least one character backward until the point, PT."
  (save-excursion
    (jcs-goto-next-backward-char pt)
    (>= (point) pt)))

(defun jcs-is-there-char-forward-point-p (pt)
  "Check if there is character forward before reachs PT."
  (save-excursion
    (jcs-goto-next-forward-char pt)
    (<= (point) pt)))

(defun jcs-is-there-char-backward-util-beginning-of-line-p ()
  "Check if there is character on the left before reaches beginning of line."
  (jcs-is-there-char-backward-point-p (jcs-get-beginning-of-line-point)))

(defun jcs-is-there-char-forward-until-end-of-line-p ()
  "Check if there is character on the right before reaches the end of line."
  (jcs-is-there-char-forward-point-p (jcs-get-end-of-line-point)))

;;
;; (@* "Symbol" )
;;

(defun jcs-print-current-symbol ()
  "Print out the current symbol."
  (interactive)
  (message "[INFO] Current symbol: %s" (jcs-get-symbol-at-point)))

(defun jcs-get-symbol-at-point ()
  "Get symbol at current cursor position."
  (thing-at-point 'symbol))

(defun jcs-kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

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

;;
;; (@* "Word" )
;;

(defun jcs-print-current-word ()
  "Print out the current word."
  (interactive)
  (message "[INFO] Current word: %s" (jcs-get-word-at-point)))

(defun jcs-get-word-at-point ()
  "Get word at current cursor position."
  (thing-at-point 'word))

(defun jcs-current-word-equal-p (str)
  "Check the current word equal to STR, STR can be a list of string."
  (cond ((stringp str)
         (string= (thing-at-point 'word) str))
        ((listp str)
         (jcs-contain-list-string str (thing-at-point 'word)))
        (t nil)))

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

;;
;; (@* "Column" )
;;

(defun jcs-column-at-pos (&optional pt)
  "Column at PT."
  (unless pt (setq pt (point)))
  (save-excursion
    (goto-char pt)
    (current-column)))

;;
;; (@* "Line" )
;;

(defun jcs-print-current-line ()
  "Print out the current line."
  (interactive)
  (message "[INFO] Current line: %s" (jcs-get-current-line-string)))

(defun jcs-goto-line (ln)
  "Goto LN line number."
  (goto-char (point-min))
  (forward-line (1- ln)))

(defun jcs-goto-first-char-in-line ()
  "Goto beginning of line but ignore 'empty characters'(spaces/tabs)."
  (jcs-back-to-indentation-or-beginning)
  (when (jcs-is-beginning-of-line-p) (jcs-back-to-indentation-or-beginning)))

(defun jcs-first-char-in-line-point ()
  "Return point in first character in line."
  (save-excursion (jcs-goto-first-char-in-line) (point)))

(defun jcs-first-char-in-line-column ()
  "Return column in first character in line."
  (save-excursion (jcs-goto-first-char-in-line) (current-column)))

(defun jcs-current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]\t]*$")))

(defun jcs-current-line-totally-empty-p ()
  "Current line empty with no spaces/tabs in there.  (absolute)."
  (and (jcs-is-beginning-of-line-p) (jcs-is-end-of-line-p)))

(defun jcs-current-line-comment-p ()
  "Check if current line only comment."
  (save-excursion
    (let ((is-comment-line nil))
      (end-of-line)
      (when (or (jcs-inside-comment-p) (jcs-current-line-empty-p))
        (setq is-comment-line t))
      is-comment-line)))

(defun jcs-get-beginning-of-line-point (&optional ln)
  "Return point at beginning of LN."
  (save-excursion (when ln (jcs-goto-line ln)) (beginning-of-line) (point)))

(defun jcs-get-end-of-line-point (&optional ln)
  "Return point at end of LN."
  (save-excursion (when ln (jcs-goto-line ln)) (end-of-line) (point)))

(defun jcs-is-beginning-of-line-p ()
  "Check if it's at the beginning of line."
  (= (current-column) 0))

(defun jcs-is-end-of-line-p ()
  "Check if it's at the end of line."
  (= (point) (jcs-get-end-of-line-point)))

(defun jcs-is-end-of-buffer-p ()
  "Check if it's at the end of buffer."
  (= (point) (point-max)))

(defun jcs-is-current-file-empty-p (&optional fn)
  "Check if the FN an empty file."
  (if fn (with-current-buffer fn
           (and (bobp) (jcs-is-end-of-buffer-p)))
    (and (bobp) (jcs-is-end-of-buffer-p))))

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
  (let ((current-point nil) (firstCharPoint nil))
    (save-excursion
      (setq current-point (point))
      (back-to-indentation)
      (setq firstCharPoint (point)))
    (= firstCharPoint current-point)))

(defun jcs-is-infront-first-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing infront of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-backward "[^ \t]" (line-beginning-position) t))))

(defun jcs-is-behind-last-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing behind of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-forward "[^ \t]" (line-end-position) t))))

(defun jcs-empty-line-between-point (min-pt max-pt)
  "Check if there is empty line between two point, MIN-PT and MAX-PT."
  (save-excursion
    (let (there-is-empty-line)
      (when (>= min-pt max-pt)
        (error "Min point cannot be larger than max point")
        there-is-empty-line)
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
  (= (line-number-at-pos (point) t) (line-number-at-pos (point-min) t)))

(defun jcs-last-line-in-buffer-p ()
  "Is current line the last line in buffer."
  (= (line-number-at-pos (point) t) (line-number-at-pos (point-max) t)))

(defun jcs-first-visible-pos-in-window ()
  "First point in current visible window."
  (save-excursion
    (ignore-errors (move-to-window-line 0))
    (line-beginning-position)))

(defun jcs-last-visible-pos-in-window ()
  "Last point in current visible window."
  (save-excursion
    (ignore-errors (move-to-window-line -1))
    (line-beginning-position)))

(defun jcs-first-visible-line-in-window ()
  "First line number in current visible window."
  (line-number-at-pos (jcs-first-visible-pos-in-window) t))

(defun jcs-last-visible-line-in-window ()
  "Last line number in current visible window."
  (line-number-at-pos (jcs-last-visible-pos-in-window) t))

(defun jcs-line-number-at-pos-relative (&optional pos rel-line)
  "Return line number relative to REL-LINE from POS.

If optional argument REL-LINE is nil; we will use first visible line instead."
  (unless rel-line (setq rel-line (jcs-first-visible-line-in-window)))
  (- (line-number-at-pos pos t) rel-line))

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
    (ignore-errors (recenter-top-bottom))))

(defun jcs-move-to-window-line-top-bottom (type)
  "Move to window line by TYPE."
  (let ((recenter-positions (jcs--recenter-positions type)))
    (move-to-window-line-top-bottom)))

;;
;; (@* "Move between button" )
;;

(defun jcs-top-most-line ()
  "Move to top of the buffer."
  (interactive)
  ;; NOTE: 0 : top-most-line, -1 : bottom-most-line
  (move-to-window-line-top-bottom 0))

(defun jcs-bottom-most-line()
  "Move to bottom of the buffer."
  (interactive)
  ;; NOTE: 0 : top-most-line, -1 : bottom-most-line
  (move-to-window-line-top-bottom -1))

;;
;; (@* "Mark" )
;;

(defun jcs-is-mark-active-p ()
  "Check if the mark active."
  (and mark-active
       (= (point) (mark))))

;;
;; (@* "Region" )
;;

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

(defun jcs-delete-region ()
  "Delete region by default value."
  (interactive)
  (when (use-region-p) (delete-region (region-beginning) (region-end))))

(defun jcs-select-region (st ed)
  "Select region from ST and ED."
  (goto-char st) (call-interactively #'set-mark-command) (goto-char ed))

(defun jcs-region-bound ()
  "Return region boundary, else default to min/max."
  (if (use-region-p) (cons (region-beginning) (region-end))
    (cons (point-min) (point-max))))

;;
;; (@* "Comment" )
;;

(defun jcs-inside-comment-p ()
  "Return non-nil if it's inside comment."
  (or (nth 4 (syntax-ppss))
      (jcs-is-current-point-face '(font-lock-comment-face
                                   tree-sitter-hl-face:comment
                                   tree-sitter-hl-face:doc
                                   hl-todo))))

(defun jcs-inside-comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (jcs-inside-comment-p)
      (nth 8 (syntax-ppss))
      (jcs-is-current-point-face 'font-lock-string-face)))

(defun jcs-goto-start-comment ()
  "Go to the start of the comment."
  (interactive)
  (while (jcs-inside-comment-p)
    (re-search-backward comment-start-skip nil t)))

(defun jcs-goto-end-comment ()
  "Go to the end of the comment."
  (interactive)
  (when (jcs-inside-comment-p)
    (forward-char 1)
    (jcs-goto-end-comment)))

(defun jcs-start-comment-point (&optional pt)
  "Point at the start of the comment point relative to PT."
  (save-excursion (when pt (goto-char pt)) (jcs-goto-start-comment) (point)))

(defun jcs-end-comment-point (&optional pt)
  "Point at the end of the comment point relative to PT."
  (save-excursion (when pt (goto-char pt)) (jcs-goto-end-comment) (point)))

(defun jcs-start-comment-symbol (&optional pt)
  "Return the starting comment symbol form the given PT."
  (when (jcs-inside-comment-p)
    (let (start-pt)
      (save-excursion
        (when pt (goto-char pt))
        (jcs-goto-start-comment)
        (progn  ; Make sure to go outside of symbol
          (re-search-backward "[ \t\r\n]" nil t)
          (unless (bobp) (forward-char 1)))
        (setq start-pt (point))
        (re-search-forward comment-start-skip (1+ (line-end-position)) t)
        (if (= start-pt (point)) nil
          (string-trim (buffer-substring start-pt (point))))))))

(defun jcs-end-comment-symbol (&optional pt)
  "Return the ending comment symbol form the given PT."
  (when (jcs-inside-comment-p)
    (let (end-pt)
      (save-excursion
        (when pt (goto-char pt))
        (jcs-goto-end-comment)
        (setq end-pt (point))
        (re-search-backward "[ \t\r\n]" (1- (line-beginning-position)) t)
        (if (= end-pt (point)) nil
          (string-trim (buffer-substring (point) end-pt)))))))

;;
;; (@* "Face" )
;;

(defun jcs-print-current-face ()
  "Print out all the faces the current cursor on."
  (interactive)
  (message "[INFO] Current faces: %s" (jcs-get-current-point-face)))

(defun jcs-get-faces-internal (pos)
  "Return the list of faces at this POS."
  (delete-dups
   (jcs-flatten-list
    (remq nil
          (list
           (get-char-property pos 'read-face-name)
           (get-char-property pos 'face)
           (plist-get (text-properties-at pos) 'face))))))

(defun jcs-get-faces (pos)
  "Get the font faces at POS."
  (require 'flycheck)
  (let ((was-flycheck flycheck-mode) (faces (jcs-get-faces-internal pos)))
    (when was-flycheck
      (flycheck-mode -1)
      (setq faces (jcs-get-faces-internal pos))
      (flycheck-mode 1))
    faces))

(defun jcs-get-current-point-face (&optional pos)
  "Get current POS's type face as string."
  (unless pos (setq pos (point)))
  (jcs-get-faces pos))

(defun jcs-is-current-point-face (in-face &optional pos)
  "Check if current POS's face the same face as IN-FACE."
  (require 'cl-lib)
  (let ((faces (jcs-get-current-point-face pos)))
    (cond ((listp faces)
           (if (listp in-face)
               (cl-some (lambda (fc) (cl-position fc faces :test 'string=)) in-face)
             (cl-position in-face faces :test 'string=)))
          (t (string= in-face faces)))))

(defun jcs-is-default-face-p (&optional pos)
  "Check default face at POS."
  (or (= (length (jcs-get-current-point-face pos)) 0)
      (and (= (length (jcs-get-current-point-face pos)) 1)
           (jcs-is-current-point-face 'hl-line))))

;;
;; (@* "Font" )
;;

(defun jcs-set-font-size (&optional new-size)
  "Set the font size to NEW-SIZE."
  (unless new-size (setq new-size jcs-default-font-size))
  (set-face-attribute 'default nil :height new-size))

(defun jcs-list-font-list ()
  "List out all the fonts available."
  (interactive)
  (jcs-log-list (font-family-list)))

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
  (not (string-equal (describe-font font) "No matching font being used")))

(defun jcs-font-lock-fontify-buffer ()
  "Refresh the syntax hightlight for whole buffer."
  (interactive)
  ;; Refresh the syntax hightlight.
  (jcs-mute-apply (call-interactively #'font-lock-fontify-buffer)))

;;
;; (@* "List" )
;;

(defun jcs-last-item-in-list (lst)
  "Return the last item in LST."
  (nth (1- (length lst)) lst))

(defun jcs-find-item-in-list-offset (lst key offset)
  "Find the item in LST using KEY with OFFSET the index."
  (require 'cl-lib)
  (unless offset (setq offset 0))
  (let ((result nil) (break-it nil) (item nil) (index 0))
    (while (and (not break-it) (< index (length lst)))
      (setq item (nth index lst))
      (when (cl-case (type-of key)
              (string (string-match-p key item))
              (symbol (equal key item))
              (integer (= key item)) (float (= key item))
              (t nil))
        (setq result (nth (+ index offset) lst))
        (setq break-it t))
      (setq index (1+ index)))
    result))

(defun jcs-length (obj)
  "Return an integer value represent the length of OBJ."
  (cond ((stringp obj) (length (string-trim obj)))
        ((bufferp obj) (length (string-trim (buffer-name obj))))
        (t obj)))

(defun jcs-list-min (lst)
  "Find minimum number in LST."
  (let (min)
    (dolist (num lst)
      (setq min (jcs-length min) num (jcs-length num))
      (if min (when (< num min) (setq min num)) (setq min num)))
    min))

(defun jcs-list-max (lst)
  "Find maximum number in LST."
  (let (max)
    (dolist (num lst)
      (setq max (jcs-length max) num (jcs-length num))
      (if max (when (> num max) (setq max num)) (setq max num)))
    max))

(defun jcs-flatten-list (lst)
  "Flatten the multiple dimensional array, LST to one dimensonal array.
For instance, '(1 2 3 4 (5 6 7 8)) => '(1 2 3 4 5 6 7 8)."
  (cond
   ((null lst) nil)
   ((atom lst) (list lst))
   (t (append (jcs-flatten-list (car lst)) (jcs-flatten-list (cdr lst))))))

(defun jcs-remove-nth-element (nth lst)
  "Remove NTH element from the LST and return the list."
  (if (zerop nth) (cdr lst)
    (let ((last (nthcdr (1- nth) lst))) (setcdr last (cddr last)) lst)))

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

(defun jcs-contain-list-string-regexp (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

This function uses `string-match-p'."
  (cl-some (lambda (elm) (string-match-p elm in-str)) in-list))

(defun jcs-contain-list-string-regexp-reverse (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

The reverse mean the check from regular expression is swapped."
  (cl-some (lambda (elm) (string-match-p in-str elm)) in-list))

(defun jcs-contain-list-string (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

This function uses `string-match-p'.
This function wrapped IN-STR with function `regexp-quote'."
  (cl-some (lambda (elm) (string-match-p (regexp-quote elm) in-str)) in-list))

(defun jcs-contain-list-integer (in-list in-int)
  "Return non-nil if IN-INT is listed in IN-LIST."
  (cl-some (lambda (elm) (= elm in-int)) in-list))

(defun jcs-contain-list-type-str (in-list in-str type)
  "Return non-nil if IN-STR is listed in IN-LIST.

Argument TYPE see function `jcs-string-compare-p' for more information."
  (cl-some (lambda (elm) (jcs-string-compare-p elm in-str type)) in-list))

;;
;; (@* "Minibuffer" )
;;

(defun jcs-minibuffer-do-stuff (fnc &rest args)
  "Execute FNC and ARGS in minibuffer the safe way."
  (if (not (active-minibuffer-window))
      (user-error "[ERROR] Minibuffer not active to do stuff: %s" fnc)
    (save-selected-window
      (select-window (active-minibuffer-window))
      (apply fnc args))))

;;
;; (@* "Mode" )
;;

(defun jcs-print-current-major-mode ()
  "Print out the current major mode."
  (interactive)
  (message "[INFO] Current major mode: %s" (symbol-name major-mode)))

(defun jcs-current-major-mode ()
  "Get current major mode."
  major-mode)

(defun jcs-is-current-major-mode-p (mns)
  "Check if this major modes MNS."
  (cond ((stringp mns) (string= (symbol-name major-mode) mns))
        ((listp mns)
         (let ((index 0) (len (length mns)) current-mode-name found)
           (while (and (< index len) (not found))
             (setq current-mode-name (nth index mns)
                   found (jcs-is-current-major-mode-p current-mode-name)
                   index (1+ index)))
           found))
        ((symbolp mns) (equal major-mode mns))
        (t nil)))

(defun jcs-is-minor-mode-enabled-p (mode-obj)
  "Check if this minor MODE-OBJ enabled in current buffer/file."
  (bound-and-true-p mode-obj)
  (if (fboundp mode-obj) (symbol-value mode-obj) nil))

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

;;
;; (@* "I/O" )
;;

(defun jcs-get-string-from-file (path)
  "Return PATH file content."
  (if (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string))
    ""))

(defun jcs-project-write-file (in-filename in-content)
  "Write to IN-FILENAME file path relative to the project root with IN-CONTENT content."
  (write-region in-content  ; Start
                nil  ; End
                (concat (jcs-project-current) in-filename)
                t  ; Overwrite?
                ))

(defun jcs-parse-ini (path)
  "Parse a .ini file by PATH."
  (let ((tmp-ini (jcs-get-string-from-file path))
        tmp-ini-list tmp-pair-list
        (tmp-keyword "") (tmp-value "")
        (count 0))
    (setq tmp-ini (split-string tmp-ini "\n"))

    (dolist (tmp-line tmp-ini)
      ;; check not comment.
      (unless (string-match-p "#" tmp-line)
        ;; Split it.
        (setq tmp-pair-list (split-string tmp-line "="))

        ;; Assign to temporary variables.
        (setq tmp-keyword (nth 0 tmp-pair-list)
              tmp-value (nth 1 tmp-pair-list))

        ;; Check empty value.
        (when (and (not (string-empty-p tmp-keyword)) tmp-value)
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

Argument INI-LIST is properties file; please use this with/after using
function `jcs-parse-ini'.

Argument IN-KEY is key use to search for value."
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

;;
;; (@* "File" )
;;

(defun jcs-get-file-name ()
  "Get current file name."
  (if (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (buffer-name)))

(defun jcs-get-file-name-capital ()
  "Get current file name capital."
  (capitalize (jcs-get-file-name)))

(defun jcs-get-file-name-uppercase ()
  "Get current file name uppercase."
  (upcase (jcs-get-file-name)))

(defun jcs-get-file-name-lowercase ()
  "Get current file name uppercase."
  (downcase (jcs-get-file-name)))

(defun jcs-get-file-name-without-extension ()
  "Get current file name without extension."
  (if (buffer-file-name)
      (file-name-sans-extension (jcs-get-file-name))
    (buffer-name)))

(defun jcs-get-file-name-without-extension-capital ()
  "Get current file name without extension capital."
  (capitalize (jcs-get-file-name-without-extension)))

(defun jcs-get-file-name-without-extension-uppercase ()
  "Get current file name without extension uppercase."
  (upcase (jcs-get-file-name-without-extension)))

(defun jcs-get-file-name-without-extension-lowercase ()
  "Get current file name without extension lowercase."
  (downcase (jcs-get-file-name-without-extension)))

(defun jcs-text-file-p (filename)
  "Check if FILENAME a text file and not binary."
  (with-current-buffer (find-file-noselect filename :no-warn)
    (prog1 (not (eq buffer-file-coding-system 'no-conversion))
      (kill-buffer))))

;;
;; (@* "Directory" )
;;

(defun jcs-get-current-dir ()
  "Return the string of current directory."
  default-directory)

(defun jcs-is-file-p (path)
  "Return non-nil if PATH is a file path."
  (and (file-exists-p path) (not (file-directory-p path))))

(defun jcs-is-directory-p (path)
  "Return non-nil if PATH is a directory path."
  (and (file-exists-p path) (file-directory-p path)))

(defun jcs-file-directory-exists-p (file-path)
  "Return non-nil if FILE-PATH does exists."
  (or (file-directory-p file-path)
      (file-exists-p file-path)))

(defun jcs-last-default-directory ()
  "Return a dedicated default directory."
  (require 'f)
  (let ((last-valid-buffer (nth 0 (jcs-valid-buffer-list))))
    (if last-valid-buffer
        (f-dirname (buffer-file-name last-valid-buffer))
      jcs-emacs-startup-directory)))

(defun jcs-up-one-dir-string (dir-path)
  "Go up one directory from DIR-PATH and return it directory string."
  (string-match "\\(.*\\)/" dir-path)  ; Remove the last directory in the path.
  (match-string 1 dir-path))

(defun jcs-get-file-name-or-last-dir-from-path (in-path &optional noerror)
  "Get the either the file name or last directory from the IN-PATH."
  (if (and (not (jcs-file-directory-exists-p in-path)) (not noerror))
      (error "Directory/File you trying get does not exists")
    (let ((split-dir-file-list-len 0) result-dir-or-file split-dir-file-list)

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

;;
;; (@* "String" )
;;

(defun jcs-print-current-string ()
  "Print out the current string at current point."
  (interactive)
  (message "[INFO] Current string: %s" (jcs-string-at-point)))

(defun jcs-string-compare-p (regexp str type &optional ignore-case)
  "Compare STR with REGEXP by TYPE.

Argument TYPE can be on of the following symbol.

  * regex - uses function `string-match-p'.  (default)
  * strict - uses function `string='.
  * prefix - uses function `string-prefix-p'.
  * suffix - uses function `string-suffix-p'.

Optional argument IGNORE-CASE is only uses when TYPE is either symbol `prefix'
or `suffix'."
  (require 'cl-lib)
  (cl-case type
    (strict (string= regexp str))
    (prefix (string-prefix-p regexp str ignore-case))
    (suffix (string-suffix-p regexp str ignore-case))
    (t (string-match-p regexp str))))

(defun jcs-hash-string (str)
  "Hash STR using md5."
  (declare (side-effect-free t))
  (md5 (replace-regexp-in-string "[[:space:]\n]+" " " str)))

(defun jcs-fill-n-char-seq (ch-seq n)
  "Fill CH-SEQ with N length."
  (let ((ch-out ch-seq))
    (if (not (stringp ch-out))
        (setq ch-out nil)
      (unless (or (numberp n) (jcs-is-positive n)) (setq n 1))
      (while (< (length ch-out) n)
        (setq ch-out (concat ch-out ch-seq))))
    (if ch-out (substring ch-out 0 n) nil)))

(defun jcs-string-repeat (str n &optional separator)
  "Return repeated STR in N times.

Optional argument SEPARATOR can be join between the STR."
  (unless separator (setq separator ""))
  (let ((output ""))
    (while (jcs-is-positive n)
      (setq output (concat output str (if (= n 1) "" separator))
            n (1- n)))
    output))

(defun jcs-inside-string-p (&optional pos)
  "Return non-nil if POS inside a string."
  (save-excursion
    (when pos (goto-char pos))
    (and (nth 3 (syntax-ppss))
         (jcs-is-current-point-face '(font-lock-string-face
                                      tree-sitter-hl-face:string)))))

(defun jcs-goto-start-of-the-string ()
  "Go to the start of the string."
  (interactive)
  (when (jcs-inside-string-p)
    (backward-char 1)
    (jcs-goto-start-of-the-string)))

(defun jcs-goto-end-of-the-string ()
  "Go to the start of the string."
  (interactive)
  (when (jcs-inside-string-p)
    (forward-char 1)
    (jcs-goto-end-of-the-string)))

(defun jcs-string-at-point (&optional pt)
  "Get the string at PT."
  (save-excursion
    (when pt
      (goto-char pt))
    (let ((ret-str nil) (st-str -1) (ed-str -1))
      (save-excursion (jcs-goto-start-of-the-string) (setq st-str (point)))
      (save-excursion (jcs-goto-end-of-the-string) (setq ed-str (point)))
      (unless (= st-str ed-str)
        (setq ret-str (buffer-substring-no-properties (1+ st-str) (1- ed-str))))
      ret-str)))

(defun jcs-string-at-line (&optional ln trim)
  "Return the string at LN.

If TRIM is non-nil, trim the string before return it."
  (save-excursion
    (jcs-goto-line ln)
    (if trim (string-trim (thing-at-point 'line)) (thing-at-point 'line))))

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

(defun jcs-last-regex-in-string (reg str)
  "Find the position in STR using REG from th end."
  (let ((pos -1) (run-it t))
    (while run-it
      (setq run-it (string-match-p reg str (1+ pos)))
      (when run-it (setq pos run-it)))
    (if (= pos -1) nil pos)))

(defun jcs-s-replace-displayable (str &optional rep)
  "Replace non-displayable character from STR.

Optional argument REP is the replacement string of non-displayable character."
  (unless rep (setq rep ""))
  (let ((result ""))
    (mapc (lambda (ch)
            (setq result (concat result
                                 (if (char-displayable-p ch) (string ch)
                                   rep))))
          str)
    result))

;;
;; (@* "Variable" )
;;

(defun jcs-setq-all-local-buffer (in-var in-val)
  "Set all the local buffer to some value.

Argument IN-VAR is input variable name as symbol.

Argument IN-VAL is input value to set to IN-VAR."
  (save-window-excursion
    (save-selected-window
      (let ((win-len (length (window-list))) (index 0))
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

;;
;; (@* "Loop" )
;;

(defun jcs-loop-times (fnc cnt &optional st)
  "Do FNC with CNT from ST."
  (unless st (setq st 0))
  (let ((index st))
    (while (< index cnt) (funcall fnc index) (setq index (1+ index)))))

;;
;; (@* "Loading" )
;;

(defmacro jcs-with-eval-after-load-multiple (files &rest body)
  "Execute BODY after one of the FILES is loaded."
  (declare (indent 1) (debug t))
  `(dolist (file ,files) (with-eval-after-load file (progn ,@body))))

;;
;; (@* "System" )
;;

(defun jcs-print-current-system ()
  "Print out the current system info."
  (interactive)
  (message "[INFO] Current system: %s - %s" (jcs-get-current-sysem) (system-name)))

(defun jcs-get-current-sysem ()
  "Return the current operating system."
  (cond (jcs-is-windows 'dos) (jcs-is-bsd 'mac) (jcs-is-linux 'unix) (t nil)))

;;
;; (@* "Process Reporter" )
;;

(defvar jcs-process-reporter nil
  "Global instance process reporter.")

(defvar jcs-process-reporter-timer nil
  "Timer for process reporter.")

(defvar jcs-process-reporter-refresh 0.1
  "Process reporter's refresh rate.")

(defun jcs-process-reporter-start (&optional msg)
  "Start global process reporter with MSG displayed."
  (jcs-process-reporter-done)
  (unless msg (setq msg ""))
  (setq jcs-process-reporter (make-progress-reporter msg))
  (setq jcs-process-reporter-timer
        (run-with-timer nil jcs-process-reporter-refresh #'jcs-process-reporter-update)))

(defun jcs-process-reporter-update (&optional value suffix)
  "Update global process reporter once."
  (when jcs-process-reporter
    (progress-reporter-update jcs-process-reporter value suffix)))

(defun jcs-process-reporter-done (&optional msg)
  "Kill global process reporter and log MSG when you are done."
  (when jcs-process-reporter
    (jcs-no-log-apply (progress-reporter-done jcs-process-reporter))
    (setq jcs-progress-reporter nil))
  (when (timerp jcs-process-reporter-timer)
    (cancel-timer jcs-process-reporter-timer)
    (setq jcs-process-reporter-timer nil)
    (jcs-no-log-apply (message msg))))

(provide 'jcs-util)
;;; jcs-util.el ends here
