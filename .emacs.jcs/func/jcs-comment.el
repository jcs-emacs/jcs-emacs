;;; jcs-comment.el --- Comment related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jcs-triple-char-comment-prefix-p (in-char)
  "Check if current line is a triple IN-CHAR style comment prefix.
For instance, `///', `---', etc."
  (save-excursion
    (let (is-comment-prefix)
      (jcs-goto-first-char-in-line)
      (forward-char 1)
      (when (jcs-current-char-equal-p in-char)
        (forward-char 1)
        (when (jcs-current-char-equal-p in-char)
          (forward-char 1)
          (when (jcs-current-char-equal-p in-char)
            (setq is-comment-prefix t))))
      is-comment-prefix)))

(defun jcs-tripple-char-comment-prefix-at-current-point-p (in-char)
  "Check if the current point is triple IN-CHAR style comment prefix.
For instance, `///', `---', etc."
  (save-excursion
    (let (is-comment-prefix-at-point)
      (when (jcs-current-char-equal-p in-char)
        (backward-char 1)
        (when (jcs-current-char-equal-p in-char)
          (backward-char 1)
          (when (jcs-current-char-equal-p in-char)
            (setq is-comment-prefix-at-point t))))
      is-comment-prefix-at-point)))


;;;###autoload
(defun jcs-toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-comment-uncomment-region-or-line ()
  "Comment line or region, if there are region select then just comment region.
Otherwise comment line."
  (interactive)
  ;; check if there are region select
  (if (and mark-active (/= (point) (mark)))
      (comment-or-uncomment-region (region-beginning) (region-end))
    ;; else we just comment one single line.
    (jcs-toggle-comment-on-line)))

;;;###autoload
(defun jcs-comment-line ()
  "Comment the current line."
  (interactive)
  (comment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-uncomment-line ()
  "Uncomment the current line."
  (interactive)
  (uncomment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-comment-region-or-line ()
  "If no region selected then just comment the line."
  (interactive)
  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (comment-region (region-beginning) (region-end))
    ;; else we just comment one single line.
    (jcs-comment-line)))

;;;###autoload
(defun jcs-uncomment-region-or-line ()
  "If no region selected then just comment the line."
  (interactive)
  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (uncomment-region (region-beginning) (region-end))
    ;; else we just uncomment one single line.
    (jcs-uncomment-line)))

(provide 'jcs-comment)
;;; jcs-comment.el ends here
