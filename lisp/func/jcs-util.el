;;; jcs-util.el --- All utilities put here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Macro" )
;;

(defmacro jcs-advice-add (symbols where &rest body)
  "Global advice-add utility."
  (declare (indent 2))
  `(cond ((listp ,symbols)
          (dolist (symbol ,symbols) (advice-add symbol ,where (lambda (&rest _) ,@body))))
         (t (advice-add ,symbols ,where (lambda (&rest _) ,@body)))))

(defmacro jcs-add-hook (hooks &rest body)
  "Global add-hook utility."
  (declare (indent 1))
  `(cond ((listp ,hooks)
          (dolist (hook ,hooks) (add-hook hook (lambda (&rest _) ,@body))))
         (t (add-hook ,hooks (lambda (&rest _) ,@body)))))

(defmacro jcs-with-gc-speed-up (&rest body)
  "Execute BODY with higher GC threshold."
  (declare (indent 0) (debug t))
  `(progn (jcs-gc-cons-threshold-speed-up t) ,@body (jcs-gc-cons-threshold-speed-up nil)))

(defmacro jcs-with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         (inhibit-point-motion-hooks t)
         after-focus-change-function
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         window-size-change-functions
         window-state-change-hook)
     ,@body))

(defmacro jcs-save-excursion (&rest body)
  "Re-implementation `save-excursion' in FNC with ARGS."
  (declare (indent 0) (debug t))
  `(let ((ln (line-number-at-pos nil t)) (col (current-column)))
     ,@body (jcs-goto-line ln) (move-to-column col)))

(defmacro jcs-point-at-pos (&rest body)
  "Execute BODY when return point."
  (declare (indent 0) (debug t))
  `(save-excursion ,@body (point)))

(defmacro jcs-save-scroll-conservatively (&rest body)
  "Execute BODY by saving value of variable `scroll-conservatively'."
  (declare (indent 0) (debug t))
  `(progn (jcs-scroll-conservatively-disable) ,@body (redisplay)
          (jcs-scroll-conservatively-enable)))

(defmacro jcs-save-window-excursion (&rest body)
  "Execute BODY without touching window's layout/settings."
  (declare (indent 0) (debug t))
  `(jcs-with-no-redisplay (jcs-window-record-once) ,@body (jcs-window-restore-once)))

(defmacro jcs-try-run (repetitions &rest body)
  "Try execute BODY with REPETITIONS of times."
  (declare (indent 1) (debug t))
  `(let ((cnt 0) break)
     (while (and (null break) (not (ignore-errors ,@body)))
       (setq cnt (1+ cnt)
             break (<= ,repetitions cnt)))))

(defmacro jcs-when-buffer-window (buffer-or-name &rest body)
  "Execute BODY in window BUFFER-OR-NAME."
  (declare (indent 1) (debug t))
  `(when-let ((win (ignore-errors (get-buffer-window-list ,buffer-or-name))))
     (with-selected-window (nth 0 win) ,@body)))

(defmacro jcs-if-buffer-window (buffer-or-name then &rest else)
  "Execute THEN in window BUFFER-OR-NAME; otherwise ELSE will be executed."
  (declare (indent 2) (debug t))
  `(if-let ((win (ignore-errors (get-buffer-window-list ,buffer-or-name))))
       (with-selected-window (nth 0 win) ,then)
     ,@else))

(defmacro jcs-with-other-window (&rest body)
  "Temporary replace all switch file functions with other window during BODY
execution."
  (declare (indent 0) (debug t))
  (require 'noflet)
  `(noflet ((switch-to-buffer (&rest args) (apply #'jcs-switch-to-buffer-other-window args))
            (find-file (&rest args) (apply #'find-file-other-window args)))
     ,@body))

;;
;; (@* "Module" )
;;

(defmacro jcs-require (feature &optional filename noerror)
  "Require FEATURE; it can be a list."
  (declare (indent -1))
  `(cond ((listp ,feature) (dolist (module ,feature) (require module ,filename ,noerror)))
         ((symbolp ,feature) (require ,feature ,filename ,noerror))
         (t (user-error "Unknown type to require %s" (type-of ,feature)))))

(defmacro jcs-with-eval-after-load (files &rest body)
  "Execute BODY after one of the FILES is loaded."
  (declare (indent 1) (debug t))
  `(cond
    ((listp ,files) (dolist (file ,files) (with-eval-after-load file ,@body)))
    (t (with-eval-after-load ,files ,@body))))

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
;; (@* "Buffer" )
;;

(defmacro jcs-with-current-buffer (buffer-or-name &rest body)
  "Safe `with-current-buffer'."
  (declare (indent 1) (debug t))
  `(when (or (buffer-live-p ,buffer-or-name) (get-buffer ,buffer-or-name))
     (with-current-buffer ,buffer-or-name ,@body)))

(defun jcs-buffer-name-or-buffer-file-name (&optional buf)
  "Return BUF's `buffer-file-name' or `buffer-name' respectively."
  (or (buffer-file-name buf) (buffer-name buf)))

(defun jcs-virtual-buffer-p (&optional buffer)
  "Return non-nil if buffer doesn't exist on disk."
  (not (jcs-valid-buffer-p buffer)))

(defun jcs-valid-buffer-p (&optional buffer)
  "Return non-nil if buffer does exist on disk."
  (when-let ((bfn (buffer-file-name buffer))) (file-exists-p bfn)))

(defun jcs-invalid-buffer-p (&optional buffer)
  "Return non-nil if buffer does't exist on disk but has a valid file path.
This occurs when file was opened but has moved to somewhere else externally."
  (when-let ((bfn (buffer-file-name buffer))) (not (file-exists-p bfn))))

(defun jcs-virtual-buffer-list ()
  "Return a list of virtual buffers."
  (cl-remove-if-not #'jcs-virtual-buffer-p (buffer-list)))

(defun jcs-valid-buffer-list ()
  "Return a list of valid buffers."
  (cl-remove-if-not #'jcs-valid-buffer-p (buffer-list)))

(defun jcs-invalid-buffer-list ()
  "Return a list of invalid buffers."
  (cl-remove-if-not #'jcs-invalid-buffer-p (buffer-list)))

(defun jcs-valid-buffers-count ()
  "Return size of the valid buffers."
  (length (jcs-valid-buffer-list)))

(defun jcs-invalid-buffers-count ()
  "Return size of the invalid buffers."
  (length (jcs-invalid-buffer-list)))

(defun jcs-walk-buffers (fnc)
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
            (`object (push buf buf-lst))
            (`string (push buf-name buf-lst))))))
    buf-lst))

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
;; (@* "Color" )
;;

(defun jcs-light-color-p (hex-code)
  "Return non-nil if HEX-CODE is in light tone."
  (when (display-graphic-p)
    (let ((gray (nth 0 (color-values "gray")))
          (color (nth 0 (color-values hex-code))))
      (< gray color))))

;;
;; (@* "Command" )
;;

(defun jcs-shell-execute (cmd &rest args)
  "Return non-nil if CMD executed succesfully with ARGS."
  (save-window-excursion
    (jcs-mute-apply
      (= 0 (shell-command (concat cmd " "
                                  (mapconcat #'shell-quote-argument args " ")))))))

;;
;; (@* "Event" )
;;

(defun jcs-last-input-event-p (te)
  "Return non-nil if `last-input-event' is TE."
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
      (when-let ((success (ignore-errors (funcall fnc))))
        (with-current-buffer (if (bufferp success) success (current-buffer))
          (list (current-buffer) (line-number-at-pos) (current-column)
                (jcs-first-visible-line-in-window)))))))

(defun jcs--record-window-excursion-apply (record)
  "Apply the RECORD from `jcs--record-window-excursion'."
  (if (not record) (user-error "[INFO] No definition found for current target")
    (select-window (get-largest-window nil nil t))
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
  `(let ((message-log-max jcs-message-log-max)) ,@body))

(defmacro jcs-mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defmacro jcs-no-log-apply (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let (message-log-max) ,@body))

(defun jcs-funcall-fboundp (fnc &rest args)
  "Call FNC with ARGS if exists."
  (when (fboundp fnc) (if args (funcall fnc args) (funcall fnc))))

;;
;; (@* "Fuzzy" )
;;

(defun jcs-sort-candidates-by-function (candidates prefix fnc &optional flip)
  "Sort CANDIDATES with PREFIX and FNC.

If optional argument FLIP is non-nil, reverse query and pattern order."
  (let ((scoring-table (ht-create)) scoring-keys)
    (dolist (cand candidates)
      (when-let*
          ((scoring (ignore-errors
                      (if flip (funcall fnc prefix cand)
                        (funcall fnc cand prefix))))
           (score (cond ((listp scoring) (nth 0 scoring))
                        ((vectorp scoring) (aref scoring 0))
                        ((numberp scoring) scoring)
                        (t 0))))
        ;; XXX ht causes unknown error on start, use regular hash table functions
        ;; for now
        (unless (gethash score scoring-table) (setf (gethash score scoring-table) nil))
        (push cand (gethash score scoring-table))))
    ;; Get all keys, and turn into a list.
    (ht-map (lambda (score-key _) (push score-key scoring-keys)) scoring-table)
    (setq scoring-keys (sort scoring-keys #'>)  ; Sort keys in order
          candidates nil)  ; Clean up, and ready for final output
    (dolist (key scoring-keys)
      (let ((cands (ht-get scoring-table key)))
        (setq candidates (append candidates cands)))))
  candidates)

;;
;; (@* "Key" )
;;

(defmacro jcs-key (keymap alist)
  "Bind ALIST to KEYMAP."
  (declare (indent 1))
  `(dolist (data ,alist)
     (let ((key (car data)) (def (cdr data)))
       (if (keymapp ,keymap) (define-key ,keymap (eval key) def)
         (user-error "[WARNING] Issue bind key `%s`, `%s`, `%s`" ,keymap key def)))))

(defmacro jcs-key-local (alist)
  "Bind ALIST to local keymap."
  (declare (indent 0))
  `(dolist (data ,alist)
     (let ((key (car data)) (def (cdr data)))
       (local-set-key (eval key) def))))

(defmacro jcs-leaf-key* (alist)
  "Bind key with ALIST using `leaf-key*'."
  (declare (indent 0))
  `(progn
     (defvar data)
     (dolist (data ,alist)
       (eval `(leaf-key* (eval (car data)) (cdr data))))))

;;
;; (@* "Timer" )
;;

(defun jcs-safe-kill-timer (tmr)
  "Kill timer (TMR) the safe way."
  (when (timerp tmr) (cancel-timer tmr) (setf tmr nil) tmr))

;;
;; (@* "Organize Code" )
;;

(defun jcs-keep-one-line-between ()
  "Keep one line between the two line of code."
  (interactive)
  (if (jcs-current-line-empty-p)
      (progn
        (forward-line 1)
        ;; Kill empty line until there is one line.
        (while (jcs-current-line-empty-p) (jcs-kill-whole-line)))
    ;; Make sure have one empty line between.
    (insert "\n")))

;;
;; (@* "Tab / Space" )
;;

(defun jcs-is-good-space-to-convert-to-tab-p ()
  "Return non-nil, when we have enough spaces to convert to a tab."
  (and (not (jcs-beginning-of-line-p))
       (jcs-current-char-equal-p " ")))

(defun jcs-convert-space-to-tab (is-forward)
  "Convert space to tab if current point is space by direction IS-FORWARD."
  (save-excursion
    (let (good-to-convert)
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
        (back-to-indentation)
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
                (not (jcs-beginning-of-line-p))
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
    (while (and (< tmp-count target-width) (not (jcs-end-of-line-p)))
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
  (jcs-current-char-equal-p '(" " "\t")))

(defun jcs-current-char-equal-p (c)
  "Check the current character equal to C, C can be a list of character."
  (cond ((and (stringp c) (stringp (jcs-get-current-char-string)))
         (string= (jcs-get-current-char-string) c))
        ((listp c) (member (jcs-get-current-char-string) c))))

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
                (or (jcs-current-whitespace-or-tab-p) (jcs-beginning-of-line-p)))
      (forward-char -1))))

(defun jcs-goto-next-forward-char (&optional bnd-pt)
  "Goto the next forward character (not include space/tab).
BND-PT : boundary point."
  (interactive)
  (unless bnd-pt (setq bnd-pt (point-max)))
  (unless (eobp)
    (forward-char 1)
    (while (and (<= (point) bnd-pt)
                (or (jcs-current-whitespace-or-tab-p) (jcs-beginning-of-line-p)))
      (forward-char 1))))

(defun jcs-first-backward-char-in-line-p (ch)
  "Check the first character on the left is CH or not, with current line as boundary."
  (save-excursion
    ;; NOTE: First fowrad a char and ready to be check for next backward character.
    (forward-char 1)
    (jcs-goto-next-backward-char (1+ (line-beginning-position)))
    (string= (jcs-get-current-char-string) ch)))

(defun jcs-first-forward-char-in-line-p (ch)
  "Check the first character on the right is CH or not with current line as boundary."
  (save-excursion
    (jcs-goto-next-forward-char (line-end-position))
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
  (jcs-is-there-char-backward-point-p (line-beginning-position)))

(defun jcs-is-there-char-forward-until-end-of-line-p ()
  "Check if there is character on the right before reaches the end of line."
  (jcs-is-there-char-forward-point-p (line-end-position)))

;;
;; (@* "Symbol" )
;;

(defun jcs-print-current-symbol ()
  "Print out the current symbol."
  (interactive)
  (message "[INFO] Current symbol: %s" (symbol-at-point)))

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
  (message "[INFO] Current word: %s" (word-at-point)))

(defun jcs-current-word-equal-p (str)
  "Check the current word equal to STR, STR can be a list of string."
  (cond ((stringp str)
         (string= (thing-at-point 'word) str))
        ((listp str)
         (member (thing-at-point 'word) str))
        (t nil)))

;;
;; (@* "Line" )
;;

(defun jcs-goto-line (ln)
  "Goto LN line number."
  (goto-char (point-min)) (forward-line (1- ln)))

(defun jcs-lines-in-region (fnc &optional beg end)
  "Execute FNC each line in region BEG to END."
  (setq beg (or beg (region-beginning))
        end (or end (region-end)))
  (jcs-with-select-region
    (goto-char beg)
    (while (and (<= (line-beginning-position) end) (not (eobp)))
      (let ((delta (line-end-position)))
        (funcall-interactively fnc)
        (setq delta (- (line-end-position) delta)
              end (+ end delta)))
      (forward-line 1))))

(defun jcs-first-char-in-line-column ()
  "Return column in first character in line."
  (save-excursion (back-to-indentation) (current-column)))

(defun jcs-current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]\t]*$")))

(defun jcs-current-line-totally-empty-p ()
  "Current line empty with no spaces/tabs in there.  (absolute)."
  (and (jcs-beginning-of-line-p) (jcs-end-of-line-p)))

(defun jcs-current-line-comment-p ()
  "Check if current line only comment."
  (save-excursion
    (let ((is-comment-line nil))
      (end-of-line)
      (when (or (jcs-inside-comment-p) (jcs-current-line-empty-p))
        (setq is-comment-line t))
      is-comment-line)))

(defun jcs-beginning-of-line-p ()
  "Return non-nil if beginning of line."
  (= (current-column) 0))

(defun jcs-end-of-line-p ()
  "Return non-nil if end of line."
  (= (point) (line-end-position)))

(defun jcs-current-file-empty-p (&optional fn)
  "Check if the FN an empty file."
  (if fn (with-current-buffer fn (and (bobp) (eobp)))
    (and (bobp) (eobp))))

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

(defun jcs-start-line-in-buffer-p ()
  "Is current line the start line in buffer."
  (= (line-number-at-pos (point) t) (line-number-at-pos (point-min) t)))

(defun jcs-last-line-in-buffer-p ()
  "Is current line the last line in buffer."
  (= (line-number-at-pos (point) t) (line-number-at-pos (point-max) t)))

(defun jcs-first-visible-line-in-window ()
  "First line number in current visible window."
  (line-number-at-pos (window-start) t))

(defun jcs-last-visible-line-in-window ()
  "Last line number in current visible window."
  (line-number-at-pos (window-end) t))

(defun jcs-line-number-at-pos-relative (&optional pos rel-line)
  "Return line number relative to REL-LINE from POS.

If optional argument REL-LINE is nil; we will use first visible line instead."
  (- (line-number-at-pos pos t) (or rel-line (jcs-first-visible-line-in-window))))

(defun jcs-make-first-visible-line-to (ln)
  "Make the first visible line to target line, LN."
  (jcs-goto-line ln) (jcs-recenter-top-bottom 'top))

(defun jcs-make-last-visible-line-to (ln)
  "Make the last visible line to target line, LN."
  (jcs-goto-line ln) (jcs-recenter-top-bottom 'bottom))

(defun jcs--recenter-positions (type)
  "Return the recenter position value by TYPE."
  (cl-case type (`top '(top)) (`middle '(middle)) (`bottom '(bottom))))

(defun jcs-recenter-top-bottom (type)
  "Recenter the window by TYPE."
  (let ((recenter-positions (jcs--recenter-positions type)))
    (ignore-errors (recenter-top-bottom))))

;;
;; (@* "Region" )
;;

(defmacro jcs-with-select-region (&rest body)
  "Execute BODY and save region state."
  (declare (indent 0) (debug t))
  `(let* ((beg (region-beginning)) (end (region-end))
          (at-beg (= (point) beg))
          (ov (make-overlay beg end)))
     (ignore-errors ,@body)
     (goto-char (if at-beg (overlay-end ov) (overlay-start ov)))
     (setq deactivate-mark nil)
     (goto-char (if at-beg (overlay-start ov) (overlay-end ov)))
     (delete-overlay ov)))

(defun jcs-delete-region ()
  "Delete region by default value."
  (interactive)
  (when (use-region-p) (delete-region (region-beginning) (region-end))))

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
      (jcs-current-point-face '(font-lock-comment-face
                                tree-sitter-hl-face:comment
                                tree-sitter-hl-face:doc
                                hl-todo))))

(defun jcs-inside-comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (jcs-inside-comment-p)
      (nth 8 (syntax-ppss))
      (jcs-current-point-face '(font-lock-string-face
                                tree-sitter-hl-face:string
                                tree-sitter-hl-face:string.special
                                tree-sitter-hl-face:escape))))

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
          (when (= (point) (line-end-position)) (forward-char 1)))
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
  (require 'dash)
  (delete-dups
   (-flatten
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

(defun jcs-current-point-face (in-face &optional pos)
  "Check if current POS's face the same face as IN-FACE."
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
           (jcs-current-point-face 'hl-line))))

;;
;; (@* "Font" )
;;

(defun jcs-set-font-size (&optional new-size)
  "Set the font size to NEW-SIZE."
  (set-face-attribute 'default nil :height (or new-size jcs-default-font-size)))

;;
;; (@* "List" )
;;

(defun jcs-last-item-in-list (lst)
  "Return the last item in LST."
  (nth (1- (length lst)) lst))

(defun jcs-find-item-in-list-offset (lst key offset)
  "Find the item in LST using KEY with OFFSET the index."
  (unless offset (setq offset 0))
  (let ((result nil) (break-it nil) (item nil) (index 0))
    (while (and (not break-it) (< index (length lst)))
      (setq item (nth index lst))
      (when (cl-case (type-of key)
              (`string (string-match-p key item))
              (`symbol (equal key item))
              (`integer (= key item)) (float (= key item)))
        (setq result (nth (+ index offset) lst)
              break-it t))
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

(defun jcs-contain-list-type-str (elt list type &optional reverse)
  "Return non-nil if ELT is listed in LIST.

Argument TYPE see function `jcs-string-compare-p' for more information.

If optional argument REVERSE is non-nil, LIST item and ELT argument."
  (cl-some
   (lambda (elm)
     (if reverse (jcs-string-compare-p elt elm type)
       (jcs-string-compare-p elm elt type)))
   list))

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

(defun jcs-minibuf--compare-p (name type)
  "Compare buffer-string with NAME and TYPE."
  (jcs-minibuffer-do-stuff (lambda () (jcs-string-compare-p name (buffer-string) type))))

;;
;; (@* "Mode" )
;;

(defun jcs-re-enable-mode-if-was-enabled (modename)
  "Re-enable the MODENAME if was enabled."
  (when (boundp modename)
    (when (symbol-value modename) (jcs-re-enable-mode modename))
    (symbol-value modename)))

(defun jcs-re-enable-mode (modename)
  "Re-enable the MODENAME."
  (jcs-mute-apply
    (funcall-interactively modename -1) (funcall-interactively modename 1)))

(defun jcs-enable-disable-mode-if (modename predicate)
  "To enable/disable the MODENAME by PREDICATE."
  (jcs-mute-apply
    (if predicate (funcall-interactively modename 1)
      (funcall-interactively modename -1))))

(defun jcs-active-minor-mode (name args)
  "Active minor mode only when it's on/off."
  (jcs-mute-apply
    (if (= args 1) (unless (symbol-value name) (funcall-interactively name 1))
      (when (symbol-value name) (funcall-interactively name -1)))))

;;
;; (@* "I/O" )
;;

(defun jcs-file-content (path)
  "Return PATH file content."
  (if (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string))
    ""))

(defun jcs-create-path-if-not-exists (path)
  "Create PATH if it doesn't exist."
  (unless (jcs-directory-p path) (make-directory path t)))

(defun jcs-move-path (path dest)
  "Move PATH to DEST."
  (jcs-create-path-if-not-exists dest)
  (jcs-shell-execute (if jcs-is-windows "move" "mv") (expand-file-name path) (expand-file-name dest)))

;;
;; (@* "File" )
;;

(defun jcs-file-name ()
  "Get current file name."
  (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) (buffer-name)))

(defun jcs-file-name-without-extension ()
  "Get current file name without extension."
  (if (buffer-file-name) (file-name-sans-extension (jcs-file-name)) (buffer-name)))

(defun jcs-text-file-p (filename)
  "Return non-nil if FILENAME is a text file and not binary."
  (with-current-buffer (find-file-noselect filename :no-warn)
    (prog1 (not (eq buffer-file-coding-system 'no-conversion))
      (kill-buffer))))

;;
;; (@* "Directory" )
;;

(defun jcs-file-p (path)
  "Return non-nil if PATH is a file path."
  (and (file-exists-p path) (not (file-directory-p path))))

(defun jcs-directory-p (path)
  "Return non-nil if PATH is a directory path."
  (and (file-exists-p path) (file-directory-p path)))

(defun jcs-file-directory-exists-p (file-path)
  "Return non-nil if FILE-PATH does exists."
  (or (file-directory-p file-path) (file-exists-p file-path)))

(defun jcs-last-default-directory ()
  "Return a dedicated default directory."
  (if-let ((buffers (nth 0 (jcs-valid-buffer-list))))
      (file-name-directory (buffer-file-name buffers))
    jcs-emacs-startup-directory))

;;
;; (@* "String" )
;;

(defun jcs-string-compare-p (regexp str type &optional ignore-case)
  "Compare STR with REGEXP by TYPE.

Argument TYPE can be on of the following symbol.

  * regex - uses function `string-match-p'.  (default)
  * strict - uses function `string='.
  * prefix - uses function `string-prefix-p'.
  * suffix - uses function `string-suffix-p'.

Optional argument IGNORE-CASE is only uses when TYPE is either symbol `prefix'
or `suffix'."
  (cl-case type
    (`strict (string= regexp str))
    (`prefix (string-prefix-p regexp str ignore-case))
    (`suffix (string-suffix-p regexp str ignore-case))
    (t (ignore-errors (string-match-p regexp str)))))

(defun jcs-fill-n-char-seq (ch-seq n)
  "Fill CH-SEQ with N length."
  (when-let ((ch-out ch-seq) (n (or n 1)))
    (while (< (length ch-out) n) (setq ch-out (concat ch-out ch-seq)))
    (when ch-out (substring ch-out 0 n))))

(defun jcs-inside-string-p (&optional pos)
  "Return non-nil if POS inside a string."
  (save-excursion
    (when pos (goto-char pos))
    (and (nth 3 (syntax-ppss))
         (jcs-current-point-face '(font-lock-string-face
                                   tree-sitter-hl-face:string)))))

(defun jcs-s-replace-displayable (str &optional rep)
  "Replace non-displayable character from STR.

Optional argument REP is the replacement string of non-displayable character."
  (let ((result "") (rep (or rep "")))
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
            ;; NOTE: this will actually set whatever the variable are. Either
            ;; global or local variable will work.
            ;;
            ;; TOPIC: Variable references in lisp
            ;; URL: https://stackoverflow.com/questions/1249991/variable-references-in-lisp
            (set in-var (symbol-value in-val)))

          ;; To next window.
          (jcs-other-window-next)
          (cl-incf index))))))

(provide 'jcs-util)
;;; jcs-util.el ends here
