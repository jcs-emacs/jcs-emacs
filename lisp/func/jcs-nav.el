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
     ((memq major-mode jcs-elisp-def-modes)
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
;; (@* "Move in lines" )
;;

(defmacro jcs--define-prev/next-key (direction body-prev &rest body-next)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(cl-case ,direction
     (`previous (progn ,body-prev))
     (`next (progn ,@body-next))
     (t (user-error "[WARNING] Please define direction with 'previous' or 'next'"))))

(defun jcs-get-prev/next-key-type (direction)
  "Return the prev/next key type by DIRECTION."
  (cl-case major-mode
    (`shell-mode
     (jcs--define-prev/next-key direction #'jcs-shell-up-key #'jcs-shell-down-key))
    (t
     (jcs--define-prev/next-key
       direction
       (cl-case jcs-prev/next-key-type
         (`normal 'previous-line)
         (`smart 'jcs-smart-previous-line)
         (t (user-error "[WARNING] Prev/Next key type not defined")))
       (cl-case jcs-prev/next-key-type
         (`normal 'next-line)
         (`smart 'jcs-smart-next-line)
         (t (user-error "[WARNING] Prev/Next key type not defined")))))))

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
  (call-interactively #'previous-line)
  (jcs-nav--after-smart-move-line))

(defun jcs-smart-next-line ()
  "Smart way to navigate to next line."
  (interactive)
  (call-interactively #'next-line)
  (jcs-nav--after-smart-move-line))

;;
;; (@* "Move Between Word (Wrapper)" )
;;

(defun jcs-smart-backward-word ()
  "Smart backward a word."
  (interactive)
  (let ((start-pt (point)) (start-ln (line-number-at-pos))
        (beg-ln (jcs-beginning-of-line-p))
        (infront-first-char (jcs-is-infront-first-char-at-line-p)))
    (backward-word 1)
    (cond ((and infront-first-char (not beg-ln))
           (goto-char start-pt)
           (beginning-of-line))
          ((and (not (= start-ln (line-number-at-pos))) (not beg-ln))
           (goto-char start-pt)
           (jcs-back-to-indentation-or-beginning))
          ((>= (abs (- start-ln (line-number-at-pos))) 2)
           (goto-char start-pt)
           (forward-line -1)
           (end-of-line)))))

(defun jcs-smart-forward-word ()
  "Smart forward a word."
  (interactive)
  (let ((start-pt (point))
        (start-ln (line-number-at-pos))
        (behind-last-char (jcs-is-behind-last-char-at-line-p)))
    (forward-word 1)
    (cond ((and (not (= start-ln (line-number-at-pos)))
                (not behind-last-char))
           (goto-char start-pt)
           (end-of-line))
          ((>= (abs (- start-ln (line-number-at-pos))) 2)
           (goto-char start-pt)
           (forward-line 1)
           (jcs-back-to-indentation-or-beginning)))))

(defun jcs-backward-word-capital ()
  "Backward search capital character and set the cursor to the point."
  (interactive)
  (let ((max-pt (save-excursion (jcs-smart-backward-word) (1+ (point)))))
    (while (and (not (bobp))
                (not (jcs-current-char-uppercasep))
                (> (point) max-pt))
      (backward-char 1))
    (backward-char 1)))

(defun jcs-forward-word-capital ()
  "Forward search capital character and set the cursor to the point."
  (interactive)
  (let ((max-pt (save-excursion (jcs-smart-forward-word) (point))))
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
  (let ((visual-line-column -1) first-line-in-non-truncate-line)
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
    ;; If before point is the same as the current point; we call regaulr
    ;; `end-of-line' function.
    (when (= before-pnt (point)) (call-interactively #'end-of-line))))

(defun jcs-beginning-of-line ()
  "Goto the beginning of line."
  (interactive)
  (if truncate-lines (call-interactively #'jcs-back-to-indentation-or-beginning)
    (call-interactively #'jcs-beginning-of-visual-line)))

(defun jcs-end-of-line ()
  "Goto the end of line."
  (interactive)
  (if truncate-lines (call-interactively #'end-of-line)
    (call-interactively #'jcs-end-of-visual-line)))

;;
;; (@* "Navigating Blank Line" )
;;

(defun jcs-previous-blank-line ()
  "Move to the previous line containing nothing but whitespaces or tabs."
  (interactive)
  (let ((sr-pt (save-excursion (re-search-backward "^[ \t]*\n" nil t))))
    (goto-char (or sr-pt (point-min)))))

(defun jcs-next-blank-line ()
  "Move to the next line containing nothing but whitespaces or tabs."
  (interactive)
  (when (jcs-current-line-empty-p) (forward-line 1))
  (let ((sr-pt (save-excursion (re-search-forward "^[ \t]*\n" nil t))))
    (goto-char (or sr-pt (point-max)))
    (when sr-pt (forward-line -1))))

;;
;; (@* "Character Navigation" )
;;

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

(provide 'jcs-nav)
;;; jcs-nav.el ends here
