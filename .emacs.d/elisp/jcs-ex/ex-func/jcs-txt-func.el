;; ========================================================================
;; $File: jcs-txt-func.el $
;; $Date: 2018-01-24 12:14:56 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Text mode functionalities.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;;---------------------------------------------
;; Electriic Pair
;;---------------------------------------------

;; `' electriic key.
(add-to-list 'electric-pair-pairs '(?\` . ?\'))

;;---------------------------------------------
;; Highlighting
;;---------------------------------------------

(defvar jcs-org-font-lock-comment-face-modes '(org-mode)
  "Revised version of `org-mode' comment face regexp.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\(#[[:blank:][:graph:]]*\\)" 1 'font-lock-comment-face)
           )'end))
      jcs-org-font-lock-comment-face-modes)

;;---------------------------------------------
;; Table
;;---------------------------------------------

(defun jcs-is-row-a-dividers ()
  "Check if current row is a dividers row.
@return t : is divider.
        nil : vice versa."
  (save-excursion
    (let ((tmp-end-of-line-point nil)
          (tmp-ret-val nil))
      (end-of-line)
      (setq tmp-end-of-line-point (point))

      (beginning-of-line)

      (while (< (point) tmp-end-of-line-point)
        (when (or (current-char-equal-p "-")
                  (current-char-equal-p "+"))
          (setq tmp-ret-val t))
        (forward-char 1))

      ;; return result.
      tmp-ret-val)))

(defun jcs-is-good-row ()
  "Check if is a good row to move the cursor up or down.
@return t : good row.
        nil : bad row."
  (and (not (current-line-empty-p))
       (not (equal (jcs-is-row-a-dividers) t))))

(defun jcs-count-current-column ()
  "Count the current cursor in which column in the table.
@return a integer which store current column number."

  (save-excursion
    (let ((tmp-column-count 0)
          (tmp-end-of-line-point nil))
      ;; If is a good row to check
      (when (jcs-is-good-row)
        (end-of-line)
        (setq tmp-end-of-line-point (point))

        (beginning-of-line)

        (while (< (point) tmp-end-of-line-point)
          (when (current-char-equal-p "|")
            ;; increament 1
            (setq tmp-column-count (1+ tmp-column-count)))
          (forward-char 1)))
      ;; return result.
      tmp-column-count)))

;;;###autoload
(defun jcs-org-table-up ()
  "Move cursor up one row if in the table."
  (interactive)
  (let ((tmp-column-count (jcs-count-current-column))
        (cycle-counter 0))
    (while (< cycle-counter tmp-column-count)
      (jcs-org-table-left)
      (setq cycle-counter (1+ cycle-counter)))))

;;;###autoload
(defun jcs-org-table-down ()
  "Move cursor down one row if in the table."
  (interactive)
  (let ((tmp-column-count (jcs-count-current-column))
        (cycle-counter 0))
    (while (< cycle-counter tmp-column-count)
      (jcs-org-table-right)
      (setq cycle-counter (1+ cycle-counter)))))

;;;###autoload
(defun jcs-org-table-left ()
  "Move cursor left one column if in the table."
  (interactive)
  ;; NOTE(jenchieh): use built-in.
  (org-shifttab))

;;;###autoload
(defun jcs-org-table-right ()
  "Move cursor right one column if in the table."
  (interactive)
  ;; NOTE(jenchieh): use built-in.
  (org-cycle))
