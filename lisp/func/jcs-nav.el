;;; jcs-nav.el --- Nagivation in file  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Definition" )
;;

(defun jcs-goto-definition ()
  "Move to definition."
  (interactive)
  (cond
   ((and (jcs--lsp-connected-p)
         (not (or (ignore-errors (lsp-goto-type-definition))
                  (ignore-errors (lsp-goto-implementation)))))
    t)
   ((derived-mode-p 'lisp-data-mode)
    (if (ignore-errors (call-interactively #'elisp-def))
        (progn (jcs-recenter-top-bottom 'middle) t)
      (user-error "[INFO] No definition found for current target")))
   ((ignore-errors (meta-view-at-point)))
   (t (dumb-jump-go-prefer-external))))

(defun jcs-goto-definition-other-window ()
  "Move to definition other window."
  (interactive)
  (let ((meta-view-display-function #'jcs-switch-to-buffer-other-window))
    (jcs--record-window-excursion-apply
     (jcs--record-window-excursion #'jcs-goto-definition))))

(defun jcs-peek-definition ()
  "Peek definition."
  (interactive)
  (require 'scrollable-quick-peek)
  (when-let* ((buf-list (buffer-list))
              (record (jcs--record-window-excursion #'jcs-goto-definition))
              (buf (nth 0 record)) (ln (nth 1 record)))
    (quick-peek--scroll-to-see)
    (quick-peek-set-spacers buf)
    (scrollable-quick-peek-show (with-current-buffer buf (buffer-string)))
    (setq scrollable-quick-peek-scroll-offset (- ln 3))
    (scrollable-quick-peek-scroll-down)
    ;; If does open the new file to peek, kill the buffer afterward.
    (unless (equal buf-list (buffer-list)) (kill-buffer buf))))

;;
;; (@* "Move Between Word (Wrapper)" )
;;

(defun jcs-smart-backward-word ()
  "Smart backward a word."
  (interactive)
  (let ((start-pt (point)) (start-ln (line-number-at-pos))
        (beg-ln (bolp))
        (infront-first-char (jcs-is-infront-first-char-at-line-p)))
    (backward-word 1)
    (cond ((and infront-first-char (not beg-ln))
           (goto-char start-pt)
           (beginning-of-line))
          ((and (not (= start-ln (line-number-at-pos))) (not beg-ln))
           (goto-char start-pt)
           (mwim-beginning-of-code-or-line))
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
           (mwim-beginning-of-code-or-line)))))

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
  (msgu-silent (when (jcs-backward-sexp) (jcs-forward-sexp))))

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
