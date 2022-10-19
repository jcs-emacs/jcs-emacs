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

(defun jcs-backward-word-capital ()
  "Backward search capital character and set the cursor to the point."
  (interactive)
  (let ((max-pt (save-excursion (vs-edit-backward-word) (1+ (point)))))
    (while (and (not (bobp))
                (not (jcs-current-char-uppercasep))
                (> (point) max-pt))
      (backward-char 1))
    (backward-char 1)))

(defun jcs-forward-word-capital ()
  "Forward search capital character and set the cursor to the point."
  (interactive)
  (let ((max-pt (save-excursion (vs-edit-forward-word) (point))))
    (forward-char 1)
    (while (and (not (eobp))
                (not (jcs-current-char-uppercasep))
                (< (point) max-pt))
      (forward-char 1))))

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

(defun jcs-toggle-backward-forward-sexp ()
  "Move to balance expression in backward/forward direction if any."
  (interactive)
  (msgu-silent (when (jcs-backward-sexp) (jcs-forward-sexp))))

(defun jcs-current-pair ()
  "Return current pair character."
  (let* ((prev (char-before))
         (next (char-after))
         (syntax-info (and prev
                           (electric-pair-syntax-info prev)))
         (syntax (car syntax-info))
         (pair (cadr syntax-info)))
    (ignore-errors (string pair))))

(defun jcs-backward-sexp ()
  "Wrapper for function `backward-sexp'."
  (interactive)
  (cond ((jcs-current-pair) (backward-sexp))
        ((save-excursion (forward-char 1) (jcs-current-pair))
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
  (cond ((save-excursion (forward-char 1) (jcs-current-pair))
         (forward-sexp))
        ((jcs-current-pair)
         (forward-char -1)
         (forward-sexp))
        (t (message "%s %s %s"
                    (propertize "[INFO] You are at the end of"
                                'face '(:foreground "cyan"))
                    "forward"
                    (propertize "sexp" 'face '(:foreground "cyan"))))))

(provide 'jcs-nav)
;;; jcs-nav.el ends here
