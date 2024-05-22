;;; jcs-util.el --- All utilities put here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Hook" )
;;

(defun jcs-as-hook (name)
  "Convert NAME to hook."
  (intern (concat (jcs-2str name) "-hook")))

;;
;; (@* "Macro" )
;;

(defmacro jcs-advice-add (symbols where &rest body)
  "Global advice-add utility."
  (declare (indent 2))
  `(cond ((listp ,symbols)
          (dolist (symbol ,symbols)
            (advice-add symbol ,where (lambda (&optional arg0 &rest args) ,@body))))
         (t (advice-add ,symbols ,where (lambda (&optional arg0 &rest args) ,@body)))))

(defmacro jcs-add-hook (hooks &rest body)
  "Global add-hook utility."
  (declare (indent 1))
  `(cond ((listp ,hooks)
          (dolist (hook ,hooks)
            (add-hook hook (lambda (&optional arg0 arg1 arg2 &rest args) ,@body))))
         (t (add-hook ,hooks (lambda (&optional arg0 arg1 arg2 &rest args) ,@body)))))

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
         window-scroll-functions
         window-size-change-functions
         window-state-change-hook)
     ,@body))

(defmacro jcs-save-excursion (&rest body)
  "Re-implementation `save-excursion' in FNC with ARGS."
  (declare (indent 0) (debug t))
  `(let ((ln (line-number-at-pos nil t)) (col (current-column)))
     ,@body (jcs-goto-line ln) (move-to-column col)))

(defmacro jcs-save-window-excursion (&rest body)
  "Execute BODY without touching window's layout/settings."
  (declare (indent 0) (debug t))
  `(jcs-with-no-redisplay (jcs-window-record-once) ,@body (jcs-window-restore-once)))

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
  `(noflet ((switch-to-buffer (&rest args) (apply #'jcs-switch-to-buffer-other-window args))
            (find-file (&rest args) (apply #'find-file-other-window args)))
     ,@body))

;;
;; (@* "Module" )
;;

(defvar jcs-module-history nil
  "History of the loaded modules.")

(defun jcs-module--path (module)
  "Return the path to the MODULE config file."
  (let ((root (concat user-emacs-directory "modules/" module)))
    (concat root "/config.el")))

(defun jcs-module-loaded-p (module)
  "Return non-nil if MODULE has been loaded."
  (member (jcs-module--path module) jcs-module-history))

(defun jcs-module-reload-all ()
  "Reload all modules."
  (dolist (module jcs-module-history)
    (load module t t)))

(defun jcs-module-load (modules &optional force)
  "Load MODULES.

If FORCE is non-nil, force load the module even it has been loaded already."
  (if (listp modules)
      (dolist (module modules) (jcs-module-load module))
    (let ((config (jcs-module--path modules)))
      (when (or force
                (not (jcs-module-loaded-p modules)))
        (push config jcs-module-history)
        (load config t t)))))

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
;; (@* "Pass" )
;;

(defun jcs-auth-source-get (host)
  "Basic value getter by HOST."
  (when-let* ((info (auth-source-search :max 1 :host host))
              (info (car info)))
    (or (plist-get info :value)
        (plist-get info :key)
        (plist-get info :secret)
        (plist-get info :password))))

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
  "Return non-nil if BUFFER doesn't exist on disk."
  (not (jcs-valid-buffer-p buffer)))

(defun jcs-valid-buffer-p (&optional buffer)
  "Return non-nil if BUFFER does exist on disk."
  (when-let ((bfn (buffer-file-name buffer))) (file-exists-p bfn)))

(defun jcs-invalid-buffer-p (&optional buffer)
  "Return non-nil if BUFFER does't exist on disk but has a valid file path.
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

;;
;; (@* "Color" )
;;

(defun jcs-light-color-p (hex-code)
  "Return non-nil if HEX-CODE is in light tone."
  (when elenv-graphic-p
    (let ((gray (nth 0 (color-values "gray")))
          (color (nth 0 (color-values hex-code))))
      (< gray color))))

;;
;; (@* "Command" )
;;

(defun jcs-shell-execute (cmd &rest args)
  "Return non-nil if CMD executed succesfully with ARGS."
  (save-window-excursion
    (msgu-silent
      (= 0 (shell-command
            (concat cmd " "
                    (mapconcat #'shell-quote-argument
                               (cl-remove-if #'s-blank-str-p args)
                               " ")))))))

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

(defun jcs-funcall-fboundp (fnc &rest args)
  "Call FNC with ARGS if exists."
  (when (fboundp fnc) (if args (funcall fnc args) (funcall fnc))))

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

(defmacro jcs-bind-key* (alist)
  "Bind key with ALIST using `bind-key*'."
  (declare (indent 0))
  `(dolist (data ,alist) (bind-key* (eval (car data)) (cdr data))))

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
;; (@* "Character" )
;;

;; TOPIC: Check if a character (not string) is lowercase, uppercase, alphanumeric?
;;
;; See https://stackoverflow.com/questions/27798296/check-if-a-character-not-string-is-lowercase-uppercase-alphanumeric

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
  (jcs-word-p (string-to-char (jcs-before-char-string))))

(defun jcs-current-char-uppercasep ()
  "Check if current character a uppercase character."
  (jcs-uppercase-p (string-to-char (jcs-before-char-string))))

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
  (cond ((and (stringp c) (stringp (jcs-before-char-string)))
         (string= (jcs-before-char-string) c))
        ((listp c) (member (jcs-before-char-string) c))))

(defun jcs-before-char-string ()
  "Return the character before cursor as a string."
  (if (char-before) (string (char-before)) ""))

(defun jcs-first-backward-char-in-line-p (ch)
  "Return t if the CH is the first character on the left in line."
  (save-excursion
    (when (re-search-backward "[^[:space:]]" (line-beginning-position) t)
      (forward-char 1)
      (string= (jcs-before-char-string) ch))))

(defun jcs-first-forward-char-in-line-p (ch)
  "Return t if the CH is the first character on the right in line."
  (save-excursion
    (when (re-search-forward "[[:space:]]*" (line-end-position) t)
      (forward-char 1)
      (string= (jcs-before-char-string) ch))))

;;
;; (@* "Word" )
;;

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

(defun jcs-space-p ()
  "Return t if the buffer uses spaces instead of tabs."
  (= (how-many "^\t" (point-min) (point-max)) 0))

(defun jcs-first-char-in-line-column ()
  "Return column in first character in line."
  (save-excursion (back-to-indentation) (current-column)))

(defun jcs-current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")))

(defun jcs-current-line-comment-p ()
  "Check if current line only comment."
  (save-excursion
    (let (is-comment-line)
      (end-of-line)
      (when (or (jcs-inside-comment-p) (jcs-current-line-empty-p))
        (setq is-comment-line t))
      is-comment-line)))

(defun jcs-current-file-empty-p (&optional fn)
  "Check if the FN an empty file."
  (if fn (with-current-buffer fn (and (bobp) (eobp)))
    (and (bobp) (eobp))))

(defun jcs-infront-first-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing infront of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-backward "[^[:space:]]" (line-beginning-position) t))))

(defun jcs-behind-last-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing behind of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-forward "[^[:space:]]" (line-end-position) t))))

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

(defun jcs-recenter-top-bottom (type)
  "Recenter the window by TYPE."
  (let ((recenter-positions `(,type))) (ignore-errors (recenter-top-bottom))))

;;
;; (@* "Region" )
;;

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

;;
;; (@* "Face" )
;;

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
  (jcs-get-faces (or pos (point))))

(defun jcs-current-point-face (in-face &optional pos)
  "Check if current POS's face the same face as IN-FACE."
  (let ((faces (jcs-get-current-point-face pos)))
    (cond ((listp faces)
           (if (listp in-face)
               (cl-some (lambda (fc) (cl-position fc faces :test 'string=)) in-face)
             (cl-position in-face faces :test 'string=)))
          (t (string= in-face faces)))))

;;
;; (@* "Font" )
;;

(defun jcs-set-font-size (&optional new-size)
  "Set the font size to NEW-SIZE."
  (set-face-attribute 'default nil :height (or new-size jcs-default-font-size)))

;;
;; (@* "List" )
;;

(defmacro jcs-push (newelt seq)
  "Push NEWELT to the ahead or back of SEQ."
  `(if (zerop (length ,seq))
       (push ,newelt ,seq)
     (list-utils-insert-after-pos ,seq (max (1- (length ,seq)) 0) ,newelt)))

(defun jcs-find-item-in-list-offset (lst key offset)
  "Find the item in LST using KEY with OFFSET the index."
  (unless offset (setq offset 0))
  (let ((index 0) result break-it item)
    (while (and (not break-it) (< index (length lst)))
      (setq item (nth index lst))
      (when (cl-case (type-of key)
              (`string (string-match-p key item))
              (`symbol (equal key item))
              (`integer (= key item)) (float (= key item)))
        (setq result (nth (+ index offset) lst)
              break-it t))
      (cl-incf index))
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
;; (@* "Mode" )
;;

(defun jcs-re-enable-mode-if-was-enabled (modename)
  "Re-enable the MODENAME if was enabled."
  (when (boundp modename)
    (when (symbol-value modename) (jcs-re-enable-mode modename))
    (symbol-value modename)))

(defun jcs-re-enable-mode (modename)
  "Re-enable the MODENAME."
  (msgu-silent
    (funcall-interactively modename -1) (funcall-interactively modename 1)))

(defun jcs-enable-disable-mode-if (modename predicate)
  "To enable/disable the MODENAME by PREDICATE."
  (msgu-silent
    (if predicate (funcall-interactively modename 1)
      (funcall-interactively modename -1))))

(defun jcs-active-minor-mode (name args)
  "Active minor mode only when it's on/off."
  (msgu-silent
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

(defun jcs-move-path (path dest)
  "Move PATH to DEST."
  (ignore-errors (make-directory dest t))
  (jcs-shell-execute (if elenv-windows "move" "mv")
                     (unless elenv-windows "-f")
                     (expand-file-name path) (expand-file-name dest)))

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
;; (@* "String" )
;;

(defun jcs-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

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

(provide 'jcs-util)
;;; jcs-util.el ends here
