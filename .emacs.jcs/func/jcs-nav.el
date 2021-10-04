;;; jcs-nav.el --- Nagivation in file  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Definition" )
;;

(defun jcs-goto-definition ()
  "Move to definition."
  (interactive)
  (let (jcs-recentf-tracking-p)
    (cond
     ((and (jcs--lsp-connected-p)
           (not (or (ignore-errors (lsp-goto-type-definition))
                    (ignore-errors (lsp-goto-implementation)))))
      t)
     ((jcs-is-current-major-mode-p jcs-elisp-def-modes)
      (if (ignore-errors (call-interactively #'elisp-def))
          (progn (jcs-recenter-top-bottom 'middle) t)
        (user-error "[INFO] No definition found for current target")))
     ((ignore-errors (meta-view-at-point)))
     (t (dumb-jump-go-prefer-external)))))

(defun jcs-goto-definition-other-window ()
  "Move to definition other window."
  (interactive)
  (let ((meta-view-display-function #'jcs-switch-to-buffer-other-window)
        jcs-recentf-tracking-p)
    (jcs--record-window-excursion-apply
     (jcs--record-window-excursion #'jcs-goto-definition))))

(defun jcs-peek-definition ()
  "Peek definition."
  (interactive)
  (require 'scrollable-quick-peek)
  (let* (jcs-recentf-tracking-p
         (buf-list (buffer-list))
         (record (jcs--record-window-excursion #'jcs-goto-definition))
         (buf (nth 0 record)) (ln (nth 1 record)))
    (when record
      (jcs-quick-peek--scroll-to-see)
      (jcs-set-quick-peek-spacers buf ln)
      (scrollable-quick-peek-show (with-current-buffer buf (buffer-string)))
      (setq scrollable-quick-peek-scroll-offset (- ln 3))
      (scrollable-quick-peek-scroll-down)
      ;; If does open the new file to peek, kill the buffer afterward.
      (unless (equal buf-list (buffer-list)) (kill-buffer buf)))))

;;
;; (@* "Move Between Line (Wrapper)" )
;;

(defun jcs-get-major-mode-prev/next-key-type (direction)
  "Return the major-mode's prev/next key type by DIRECTION."
  (require 'cl-lib)
  (require 'jcs-python-mode)
  (cl-case direction
    (previous
     (cond
      ((jcs-is-current-major-mode-p '("cmake-mode"
                                      "cobol-mode"
                                      "dockerfile-mode"
                                      "makefile-mode"
                                      "masm-mode"
                                      "nasm-mode"
                                      "python-mode"
                                      "yaml-mode"))
       'jcs-py-indent-up)
      ((jcs-is-current-major-mode-p '("bat-mode"
                                      "conf-javaprop-mode"
                                      "gitattributes-mode"
                                      "gitconfig-mode"
                                      "gitignore-mode"
                                      "ini-mode"
                                      "message-mode"
                                      "reb-mode"
                                      "ssass-mode"
                                      "sql-mode"
                                      "vimrc-mode"))
       'previous-line)
      ((jcs-is-current-major-mode-p '("csharp-mode"))
       'jcs-csharp-smart-indent-up)
      ((jcs-is-current-major-mode-p '("css-mode"))
       'jcs-css-smart-indent-up)
      ((jcs-is-current-major-mode-p '("shell-mode"))
       'jcs-shell-up-key)
      (t 'jcs-smart-indent-up)))
    (next
     (cond
      ((jcs-is-current-major-mode-p '("cmake-mode"
                                      "cobol-mode"
                                      "dockerfile-mode"
                                      "makefile-mode"
                                      "masm-mode"
                                      "nasm-mode"
                                      "python-mode"
                                      "yaml-mode"))
       'jcs-py-indent-down)
      ((jcs-is-current-major-mode-p '("bat-mode"
                                      "conf-javaprop-mode"
                                      "gitattributes-mode"
                                      "gitconfig-mode"
                                      "gitignore-mode"
                                      "ini-mode"
                                      "message-mode"
                                      "reb-mode"
                                      "ssass-mode"
                                      "sql-mode"
                                      "vimrc-mode"))
       'next-line)
      ((jcs-is-current-major-mode-p '("csharp-mode"))
       'jcs-csharp-smart-indent-down)
      ((jcs-is-current-major-mode-p '("css-mode"))
       'jcs-css-smart-indent-down)
      ((jcs-is-current-major-mode-p '("shell-mode"))
       'jcs-shell-down-key)
      (t 'jcs-smart-indent-down)))
    (t (user-error "[WARNING] Please define direction with 'previous' or 'next'"))))

(defun jcs-get-prev/next-key-type (direction)
  "Return the prev/next key type by DIRECTION."
  (require 'cl-lib)
  (cl-case direction
    (previous (cl-case jcs-prev/next-key-type
                (normal 'previous-line)
                (indent (jcs-get-major-mode-prev/next-key-type direction))
                (smart 'jcs-smart-previous-line)
                (t (user-error "[WARNING] Prev/Next key type not defined"))))
    (next (cl-case jcs-prev/next-key-type
            (normal 'next-line)
            (indent (jcs-get-major-mode-prev/next-key-type direction))
            (smart 'jcs-smart-next-line)
            (t (user-error "[WARNING] Prev/Next key type not defined"))))
    (t (user-error "[WARNING] Please define direction with 'previous' or 'next'"))))

(defun jcs-previous-line ()
  "Calling `previous-line' does not execute.
Just use this without remember Emacs Lisp function."
  (interactive)
  (call-interactively #'previous-line))

(defun jcs-next-line ()
  "Calling `next-line' does not execute.
Just use this without remember Emacs Lisp function."
  (interactive)
  (call-interactively #'next-line))

(defun jcs-nav--after-smart-move-line ()
  "Do stuff after smart move line."
  (cond ((jcs-current-line-empty-p)
         (end-of-line))
        ((and (jcs-is-infront-first-char-at-line-p)
              (re-search-forward "[^[:space:]\t]" (line-end-position) t))
         (forward-char -1))))

(defun jcs-smart-previous-line ()
  "Smart way to navigate to previous line."
  (interactive)
  (jcs-previous-line)
  (jcs-nav--after-smart-move-line))

(defun jcs-smart-next-line ()
  "Smart way to navigate to next line."
  (interactive)
  (jcs-next-line)
  (jcs-nav--after-smart-move-line))

;;
;; (@* "Move Between Word (Wrapper)" )
;;

(defun jcs-backward-word ()
  "Backward a word."
  (interactive)
  (call-interactively #'backward-word))

(defun jcs-forward-word ()
  "Forward a word."
  (interactive)
  (call-interactively #'forward-word))

(defun jcs-smart-backward-word ()
  "Smart backward a word."
  (interactive)
  (let ((start-pt (point)) (start-ln (line-number-at-pos))
        (beg-ln (jcs-is-beginning-of-line-p))
        (infront-first-char (jcs-is-infront-first-char-at-line-p)))
    (jcs-backward-word)
    (cond ((and infront-first-char (not beg-ln))
           (goto-char start-pt)
           (beginning-of-line))
          ((and (not (= start-ln (line-number-at-pos))) (not beg-ln))
           (goto-char start-pt)
           (jcs-back-to-indentation-or-beginning))
          ((>= (jcs-to-positive (- start-ln (line-number-at-pos))) 2)
           (goto-char start-pt)
           (forward-line -1)
           (end-of-line)))))

(defun jcs-smart-forward-word ()
  "Smart forward a word."
  (interactive)
  (let ((start-pt (point))
        (start-ln (line-number-at-pos))
        (behind-last-char (jcs-is-behind-last-char-at-line-p)))
    (jcs-forward-word)
    (cond ((and (not (= start-ln (line-number-at-pos)))
                (not behind-last-char))
           (goto-char start-pt)
           (end-of-line))
          ((>= (jcs-to-positive (- start-ln (line-number-at-pos))) 2)
           (goto-char start-pt)
           (forward-line 1)
           (jcs-back-to-indentation-or-beginning)))))

(defun jcs-backward-word-capital ()
  "Backward search capital character and set the cursor to the point."
  (interactive)
  (let ((max-pt -1))
    (save-excursion (jcs-smart-backward-word) (setq max-pt (1+ (point))))
    (while (and (not (bobp))
                (not (jcs-current-char-uppercasep))
                (> (point) max-pt))
      (backward-char 1))
    (backward-char 1)))

(defun jcs-forward-word-capital ()
  "Forward search capital character and set the cursor to the point."
  (interactive)
  (let ((max-pt -1))
    (save-excursion (jcs-smart-forward-word) (setq max-pt (point)))
    (forward-char 1)
    (while (and (not (eobp))
                (not (jcs-current-char-uppercasep))
                (< (point) max-pt))
      (forward-char 1))))

;;
;; (@* "Move Inside Line" )
;;

(defun jcs--indentation-point ()
  "Return the indentation point at the current line."
  (save-excursion (call-interactively #'back-to-indentation) (point)))

(defun jcs-back-to-indentation-or-beginning ()
  "Toggle between first character and beginning of line."
  (interactive)
  (if (= (point) (jcs--indentation-point)) (beginning-of-line) (back-to-indentation)))

(defun jcs-beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation.
If you rather it go to beginning-of-line first and to indentation on the
next hit use this version instead."
  (interactive)
  (if (bolp) (beginning-of-line) (back-to-indentation)))

(defun jcs-back-to-indentation ()
  "Back to identation by checking first character in the line."
  (interactive)
  (beginning-of-line)
  (unless (jcs-current-line-totally-empty-p) (forward-char 1))
  (while (jcs-current-whitespace-or-tab-p) (forward-char 1))
  (backward-char 1))

(defun jcs-beginning-of-visual-line ()
  "Goto the beginning of visual line."
  (interactive)
  (let ((first-line-in-non-truncate-line nil)
        (visual-line-column -1))
    ;; First, record down the beginning of visual line point.
    (save-excursion
      (call-interactively #'beginning-of-visual-line)
      (setq visual-line-column (current-column)))

    ;; Check if is the first line of non-truncate line mode.
    (when (= visual-line-column 0)
      (setq first-line-in-non-truncate-line t))

    (if first-line-in-non-truncate-line
        (call-interactively #'jcs-back-to-indentation-or-beginning)
      (let ((before-pnt (point)))
        (call-interactively #'beginning-of-visual-line)

        ;; If before point is the same as the current point.
        ;; We call regaulr `beginning-of-line' function.
        (when (= before-pnt (point))
          (call-interactively #'jcs-back-to-indentation-or-beginning))))))

(defun jcs-end-of-visual-line()
  "Goto the end of visual line."
  (interactive)
  (let ((before-pnt (point)))
    (call-interactively #'end-of-visual-line)

    ;; If before point is the same as the current point.
    ;; We call regaulr `end-of-line' function.
    (when (= before-pnt (point))
      (call-interactively #'end-of-line))))

(defun jcs-beginning-of-line ()
  "Goto the beginning of line."
  (interactive)
  (if truncate-lines
      (call-interactively #'jcs-back-to-indentation-or-beginning)
    (call-interactively #'jcs-beginning-of-visual-line)))

(defun jcs-end-of-line ()
  "Goto the end of line."
  (interactive)
  (if truncate-lines
      (call-interactively #'end-of-line)
    (call-interactively #'jcs-end-of-visual-line)))

(defun jcs-beginning-of-buffer ()
  "Goto the beginning of buffer."
  (interactive)
  (jcs-mute-apply (call-interactively #'beginning-of-buffer)))

(defun jcs-end-of-buffer ()
  "Goto the end of buffer."
  (interactive)
  (jcs-mute-apply (call-interactively #'end-of-buffer)))

;;
;; (@* "Navigating Blank Line" )
;;

(defun jcs-goto-char (pt)
  "Goto char with interactive flag enabled."
  (execute-extended-command (- pt (point)) "forward-char"))

(defun jcs-previous-blank-line ()
  "Move to the previous line containing nothing but whitespaces or tabs."
  (interactive)
  (let ((sr-pt (save-excursion (re-search-backward "^[ \t]*\n" nil t))))
    (jcs-goto-char (if sr-pt sr-pt (point-min)))))

(defun jcs-next-blank-line ()
  "Move to the next line containing nothing but whitespaces or tabs."
  (interactive)
  (when (jcs-current-line-empty-p) (forward-line 1))
  (let ((sr-pt (save-excursion (re-search-forward "^[ \t]*\n" nil t))))
    (jcs-goto-char (if sr-pt sr-pt (point-max)))
    (when sr-pt (forward-line -1))))

;;
;; (@* "Character Navigation" )
;;

(defun jcs-safe-backward-char (n)
  "Safe way to move backward N characters."
  (ignore-errors (backward-char n)))

(defun jcs-safe-forward-char (n)
  "Safe way to move forward N characters."
  (ignore-errors (forward-char n)))

(defun jcs-move-to-forward-a-char (ch)
  "Move forward to a character CH, can be regular expression."
  (ignore-errors
    (forward-char 1)
    (while (and (not (string-match-p ch (jcs-get-current-char-string)))
                (not (eobp)))
      (forward-char 1))))

(defun jcs-move-to-backward-a-char (ch)
  "Move backward to a character CH, can be regular expression."
  (ignore-errors
    (while (and (not (string-match-p ch (jcs-get-current-char-string)))
                (not (bobp)))
      (backward-char 1))
    (backward-char 1)))

(defun jcs-move-to-forward-a-word (word)
  "Move forward to a WORD."
  (forward-word 1)
  (while (and (not (jcs-current-word-equal-p word))
              (not (eobp)))
    (forward-word 1)))

(defun jcs-move-to-backward-a-word (word)
  "Move backward to a WORD."
  (backward-word 1)
  (while (and (not (jcs-current-word-equal-p word))
              (not (bobp)))
    (backward-word 1)))

;; TODO: The naming logic here is very weird..
;; Consider changing it.
(defun jcs-move-to-forward-a-char-do-recursive (ch &optional no-rec)
  "Move forward to a character and recusrive?
CH : character we target to move toward.
as NO-REC : recursive? (Default: do recusrive method)"
  (if no-rec
      (jcs-move-to-forward-a-char ch)
    (jcs-move-to-forward-a-char-recursive ch)))

;; TODO: The naming logic here is very weird..
;; Consider changing it.
(defun jcs-move-to-backward-a-char-do-recursive (ch &optional no-rec)
  "Move backward to a character and recusrive?
CH : character we target to move toward.
as NO-REC : recursive? (Default: do recusrive method)"
  (if no-rec
      (jcs-move-to-backward-a-char ch)
    (jcs-move-to-backward-a-char-recursive ch)))

;;
;; (@* "Symbol Navigation" )
;;

(defun jcs-backward-symbol (arg)
  (interactive "p")
  (forward-symbol (- arg)))

(defun jcs-forward-symbol (arg)
  (interactive "p")
  (forward-symbol arg))

;;
;; (@* "Navigating to a Character" )
;;

(defvar jcs-search-trigger-forward-char nil
  "Trigger search forward character.")
(defvar jcs-search-trigger-backward-char nil
  "Trigger search backward character.")

(defun jcs-move-to-forward-a-char-recursive (ch)
  "Move forward to a character.
CH : character we target to move toward."
  (let ((start-pt -1))
    (when jcs-search-trigger-forward-char
      (goto-char (point-min)))

    (setq jcs-search-trigger-backward-char nil
          jcs-search-trigger-forward-char nil
          start-pt (point))
    (jcs-move-to-forward-a-char ch)

    (when (eobp)
      (setq jcs-search-trigger-forward-char t)
      (goto-char start-pt)
      (message "%s %s"
               (propertize
                "Failing overwrap jcs-move-to-forward-a-char-recursive:"
                'face '(:foreground "cyan"))
               ch))))

(defun jcs-move-to-backward-a-char-recursive (ch)
  "Move backward to a character.
CH : character we target to move toward."
  (let ((start-pt -1))
    (when jcs-search-trigger-backward-char
      (goto-char (point-max)))

    (setq jcs-search-trigger-backward-char nil
          jcs-search-trigger-forward-char nil
          start-pt (point))
    (jcs-move-to-backward-a-char ch)

    (when (bobp)
      (setq jcs-search-trigger-backward-char t)
      (goto-char start-pt)
      (message "%s %s"
               (propertize
                "Failing overwrap jcs-move-to-backward-a-char-recursive:"
                'face '(:foreground "cyan"))
               ch))))

;;
;; (@* "Move toggle Open and Close all kind of character" )
;;

(defvar jcs-search-trigger-forward-open-close-char 0
  "Trigger search forward open and close character.")
(defvar jcs-search-trigger-backward-open-close-char 0
  "Trigger search backward open and close character.")

(defun jcs-move-forward-open-close-epair (openChar closeChar)
  "Move forward to a open/close parenthesis."
  (save-window-excursion
    ;; No matter what reset the forward trigger b/c we are doing
    ;; backward search now.
    (setq jcs-search-trigger-backward-open-close-char 0)

    (let ((point-before-do-anything (point))
          (point-after-look-open-char -1)
          (point-end-of-buffer -1)
          (point-after-look-close-char -1))
      (when (looking-at openChar)
        (forward-char 1))
      (ignore-errors (while (not (looking-at openChar)) (forward-char 1)))
      (setq point-after-look-open-char (point))

      ;; back to where it start
      (goto-char point-before-do-anything)

      (when (looking-at closeChar)
        (forward-char 1))
      (ignore-errors (while (not (looking-at closeChar)) (forward-char 1)))
      (setq point-after-look-close-char (point))

      ;; record down point max and point after look
      (goto-char (point-max))
      (setq point-end-of-buffer (point))

      ;; back to where it start
      (goto-char point-before-do-anything)

      (if (> point-after-look-open-char point-after-look-close-char)
          (goto-char point-after-look-close-char)
        (goto-char point-after-look-open-char))

      (when (= jcs-search-trigger-forward-open-close-char 1)
        (goto-char (point-min))
        (setq jcs-search-trigger-forward-open-close-char 0)
        (jcs-move-forward-open-close-epair openChar closeChar))

      (when (and (= point-after-look-open-char point-end-of-buffer)
                 (= point-after-look-close-char point-end-of-buffer))
        (goto-char point-before-do-anything)
        (setq jcs-search-trigger-forward-open-close-char 1)
        (message "%s %s %s %s"
                 (propertize
                  "Failing overwrap jcs-move-forward-open-close-epair:"
                  'face '(:foreground "cyan"))
                 openChar
                 (propertize "and" 'face '(:foreground "cyan"))
                 closeChar)))))

(defun jcs-move-backward-open-close-epair (openChar closeChar)
  "Move backward to a open/close parenthesis."
  (save-window-excursion
    ;; No matter what reset the forward trigger b/c we are doing
    ;; backward search now.
    (setq jcs-search-trigger-forward-open-close-char 0)

    (let ((point-before-do-anything (point))
          (point-after-look-open-char -1)
          (point-beginning-of-buffer -1)
          (point-after-look-close-char -1))
      (when (looking-at openChar)
        (forward-char -1))
      (ignore-errors  (while (not (looking-at openChar)) (backward-char 1)))
      (setq point-after-look-open-char (point))

      ;; back to where it start
      (goto-char point-before-do-anything)

      (when (looking-at closeChar)
        (forward-char -1))
      (ignore-errors  (while (not (looking-at closeChar)) (backward-char 1)))
      (setq point-after-look-close-char (point))

      ;; record down point max and point after look
      (goto-char (point-min))
      (setq point-beginning-of-buffer (point))

      ;; back to where it start
      (goto-char point-before-do-anything)

      (if (> point-after-look-open-char point-after-look-close-char)
          (goto-char point-after-look-open-char)
        (goto-char point-after-look-close-char))

      (when (= jcs-search-trigger-backward-open-close-char 1)
        (goto-char (point-max))
        (setq jcs-search-trigger-backward-open-close-char 0)
        (jcs-move-backward-open-close-epair openChar closeChar))

      (when (and (= point-after-look-open-char point-beginning-of-buffer)
                 (= point-after-look-close-char point-beginning-of-buffer))
        (goto-char point-before-do-anything)
        (setq jcs-search-trigger-backward-open-close-char 1)
        (message "%s %s %s %s"
                 (propertize
                  "Failing overwrap jcs-move-backward-open-close-epair:"
                  'face '(:foreground "cyan"))
                 openChar
                 (propertize "and" 'face '(:foreground "cyan"))
                 closeChar)))))

;;
;; (@* "Balanced Expression (sexp)" )
;;

(defvar jcs-sexp-open-chars '("(" "{" "`" "\"" "'" "[" "<")
  "List of open balanced expression.")

(defvar jcs-sexp-close-chars '(")" "}" "`" "\"" "'" "]" ">")
  "List of close balanced expression.")

(defun jcs-toggle-backward-forward-sexp ()
  "Move to balance expression in backward/forward direction if any."
  (interactive)
  (jcs-mute-apply (when (jcs-backward-sexp) (jcs-forward-sexp))))

(defun jcs-backward-sexp ()
  "Wrapper for function `backward-sexp'."
  (interactive)
  (cond ((jcs-current-char-equal-p jcs-sexp-close-chars)
         (backward-sexp))
        ((save-excursion
           (forward-char 1)
           (jcs-current-char-equal-p jcs-sexp-close-chars))
         (forward-char 1)
         (backward-sexp))
        (t (message "%s %s %s"
                    (propertize "[INFO] You are at the end of"
                                'face '(:foreground "cyan"))
                    "backward"
                    (propertize "sexp" 'face '(:foreground "cyan"))))))

(defun jcs-forward-sexp ()
  "Wrapper for function `forward-sexp'."
  (interactive)
  (cond ((save-excursion
           (forward-char 1)
           (jcs-current-char-equal-p jcs-sexp-open-chars))
         (forward-sexp))
        ((jcs-current-char-equal-p jcs-sexp-open-chars)
         (forward-char -1)
         (forward-sexp))
        (t (message "%s %s %s"
                    (propertize "[INFO] You are at the end of"
                                'face '(:foreground "cyan"))
                    "forward"
                    (propertize "sexp" 'face '(:foreground "cyan"))))))

;;
;; (@* "Move toggle Open and Close all kind of parenthesis" )
;;

(defun jcs-move-forward-open-close-paren ()
  "Move forward to a open/close parenthesis."
  (interactive)
  (jcs-move-forward-open-close-epair "(" ")"))

(defun jcs-move-backward-open-close-paren ()
  "Move backward to a open/close parenthesis."
  (interactive)
  (jcs-move-backward-open-close-epair "(" ")"))

(defun jcs-move-forward-open-close-sqr-paren ()
  "Move forward to a open/close square parenthesis."
  (interactive)
  (jcs-move-forward-open-close-epair "[[]" "]"))

(defun jcs-move-backward-open-close-sqr-paren ()
  "Move backward to a open/close square parenthesis."
  (interactive)
  (jcs-move-backward-open-close-epair "[[]" "]"))

(defun jcs-move-forward-open-close-curly-paren ()
  "Move forward to a open/close curly parenthesis."
  (interactive)
  (jcs-move-forward-open-close-epair "{" "}"))

(defun jcs-move-backward-open-close-curly-paren ()
  "Move backward to a open/close curly parenthesis."
  (interactive)
  (jcs-move-backward-open-close-epair "{" "}"))

;;
;; (@* "Single Quotation Mark" )
;;

(defun jcs-move-forward-single-quote (&optional no-rec)
  "Move forward to a single quotation mark."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "'" no-rec))

(defun jcs-move-backward-single-quote (&optional no-rec)
  "Move backward to a single quotation mark."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "'" no-rec))

;;
;; (@* "Double Quotation Mark" )
;;

(defun jcs-move-forward-double-quote (&optional no-rec)
  "Move forward to a double quotation mark."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "\"" no-rec))

(defun jcs-move-backward-double-quote (&optional no-rec)
  "Move backward to a double quotation mark."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "\"" no-rec))

;;
;; (@* "Open Parenthesis" )
;;

(defun jcs-move-forward-open-paren (&optional no-rec)
  "Move forward to a open parenthesis."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "(" no-rec))

(defun jcs-move-backward-open-paren (&optional no-rec)
  "Move backward to a open parenthesis."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "(" no-rec))

;;
;; (@* "Close Parenthesis" )
;;

(defun jcs-move-forward-close-paren (&optional no-rec)
  "Move forward to a close parenthesis."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive ")" no-rec))

(defun jcs-move-backward-close-paren (&optional no-rec)
  "Move backward to a close parenthesis."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ")" no-rec))

;;
;; (@* "Open Square Parenthesis" )
;;

(defun jcs-move-forward-open-sqr-paren (&optional no-rec)
  "Move forward to a open square parenthesis."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "[" no-rec))

(defun jcs-move-backward-open-sqr-paren (&optional no-rec)
  "Move backward to a open square parenthesis."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "[" no-rec))

;;
;; (@* "Close Square Parenthesis" )
;;

(defun jcs-move-forward-close-sqr-paren (&optional no-rec)
  "Move forward to a close square parenthesis."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "]" no-rec))

(defun jcs-move-backward-close-sqr-paren (&optional no-rec)
  "Move backward to a close square parenthesis."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "]" no-rec))

;;
;; (@* "Open Curly Parenthesis" )
;;

(defun jcs-move-forward-open-curly-paren (&optional no-rec)
  "Move forward to a open curly parenthesis."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "{" no-rec))

(defun jcs-move-backward-open-curly-paren (&optional no-rec)
  "Move backward to a open curly parenthesis."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "{" no-rec))

;;
;; (@* "Close Curly Parenthesis" )
;;

(defun jcs-move-forward-close-curly-paren (&optional no-rec)
  "Move forward to a close curly parenthesis."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "}" no-rec))

(defun jcs-move-backward-close-curly-paren (&optional no-rec)
  "Move backward to a close curly parenthesis."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "}" no-rec))

;;
;; (@* "Colon" )
;;

(defun jcs-move-forward-colon (&optional no-rec)
  "Move forward to a colon."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive ":" no-rec))

(defun jcs-move-backward-colon (&optional no-rec)
  "Move backward to a colon."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ":" no-rec))

;;
;; (@* "Semicolon" )
;;

(defun jcs-move-forward-semicolon (&optional no-rec)
  "Move forward to a semicolon."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive ";" no-rec))

(defun jcs-move-backward-semicolon (&optional no-rec)
  "Move backward to a semicolon."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ";" no-rec))

;;
;; (@* "Geater than sign" )
;;

(defun jcs-move-forward-greater-than-sign (&optional no-rec)
  "Move forward to a greater than sign."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive ">" no-rec))

(defun jcs-move-backward-greater-than-sign (&optional no-rec)
  "Move backward to a greater than sign."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive ">" no-rec))

;;
;; (@* "Less than sign" )
;;

(defun jcs-move-forward-less-than-sign (&optional no-rec)
  "Move forward to a less than sign."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "<" no-rec))

(defun jcs-move-backward-less-than-sign (&optional no-rec)
  "Move backward to a less than sign."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "<" no-rec))

;;
;; (@* "Comma" )
;;

(defun jcs-move-forward-comma (&optional no-rec)
  "Move forward to a comma."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "," no-rec))

(defun jcs-move-backward-comma (&optional no-rec)
  "Move backward to a comma."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "," no-rec))

;;
;; (@* "Period" )
;;

(defun jcs-move-forward-period (&optional no-rec)
  "Move forward to a period."
  (interactive)
  (jcs-move-to-forward-a-char-do-recursive "[.]" no-rec))

(defun jcs-move-backward-period (&optional no-rec)
  "Move backward to a period."
  (interactive)
  (jcs-move-to-backward-a-char-do-recursive "[.]" no-rec))

(provide 'jcs-nav)
;;; jcs-nav.el ends here
