;; This is the start of jcs-txt-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-txt-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Wed Jan 24 12:14:56 EST 2018>
;; Time-stamp: <2018-01-24 12:14:56>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright © 2018 Jen-Chieh Shen

;; jcs-txt-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-txt-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

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


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-txt-func.el file
