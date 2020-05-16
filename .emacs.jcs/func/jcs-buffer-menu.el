;;; jcs-buffer-menu.el --- Functions in buffer menu mode. (*Buffer List*).  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; TOPIC: BufferMenuPlus
;; URL: https://www.emacswiki.org/emacs/BufferMenuPlus

(defun jcs-buffer-menu-sort (type)
  "Sort the buffer menu by TYPE.
Sorted by (1) visit, (2) buffer, (3) size, (4) time, (5) mode, (6) file."
  (Buffer-menu-sort type)
  (goto-char (point-min)))

;;;###autoload
(defun jcs-buffer-menu-sort-by-visit ()
  "Sort the Buffer Menu List by visit."
  (interactive)
  (jcs-buffer-menu-sort 1))

;;;###autoload
(defun jcs-buffer-menu-sort-by-buffer ()
  "Sort the Buffer Menu List by buffer."
  (interactive)
  (jcs-buffer-menu-sort 2))

;;;###autoload
(defun jcs-buffer-menu-sort-by-size ()
  "Sort the Buffer Menu List by size."
  (interactive)
  (jcs-buffer-menu-sort 3))

;;;###autoload
(defun jcs-buffer-menu-sort-by-time ()
  "Sort the Buffer Menu List by time."
  (interactive)
  (jcs-buffer-menu-sort 4))

;;;###autoload
(defun jcs-buffer-menu-sort-by-mode ()
  "Sort the Buffer Menu List by mode."
  (interactive)
  (jcs-buffer-menu-sort 5))

;;;###autoload
(defun jcs-buffer-menu-sort-by-file ()
  "Sort the Buffer Menu List by file name."
  (interactive)
  (jcs-buffer-menu-sort 6))


;;;###autoload
(defun jcs-buffer-menu-return ()
  "Implemenetation for `buffer menu`'s return key."
  (interactive)
  (if jcs--buffer-menu--done-filtering
      (if (ignore-errors (Buffer-menu-this-window))
          (message nil)  ; Use to clear `[Display not ready]'.
        (user-error "No buffer on this line"))
    (setq jcs--buffer-menu-return-delay t)
    (message "[Display not ready]")))


(defvar jcs--buffer-menu--score-standard 0
  "Standard score that minimum to reach, or else delete it.
From scale 0 to 100.")

(defvar jcs--buffer-menu--done-filtering t
  "Flag to check if done filtering.")

(defvar jcs--buffer-menu--filter-timer nil
  "Store filter timer function.")

(defvar jcs--buffer-menu--filter-delay 0.1
  "Filter delay time.")

(defvar jcs--buffer-menu--pattern ""
  "Search pattern.")


(defun jcs--buffer-menu--header-appearing-p ()
  "Check if header appearing in the buffer."
  (let ((header-appear nil))
    (jcs-do-stuff-if-buffer-exists
     "*Buffer List*"
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (setq header-appear (not (tabulated-list-get-entry))))))
    header-appear))

(defun jcs--safe-print-fake-header ()
  "Safe way to print fake header."
  (when (and (not (jcs--buffer-menu--header-appearing-p))
             jcs--buffer-menu--fake-header-already-appears)
    (tabulated-list-print-fake-header)))

(defun jcs--buffer-menu-clean ()
  "Clean all the menu list."
  (goto-char (point-min))
  (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
    (if (tabulated-list-get-id)
        (tabulated-list-delete-entry)
      (forward-line 1))))

(defun jcs--buffer-menu-filter-list ()
  "Do filtering the buffer list."
  (require 'flx)
  (with-current-buffer "*Buffer List*"
    (let ((scoring-table (make-hash-table))
          (scoring-keys '()))
      (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
        (let* ((id (tabulated-list-get-id))
               (entry (tabulated-list-get-entry))
               (buf-name (buffer-name id))
               (scoring (flx-score buf-name jcs--buffer-menu--pattern))
               ;; Ensure score is not `nil'.
               (score (if scoring (nth 0 scoring) 0)))
          (when (arrayp entry)
            ;; For first time access score with hash-table, setup empty array.
            (unless (gethash score scoring-table) (setf (gethash score scoring-table) '()))
            ;; Push the candidate with the target score to hash-table.
            (push (cons id entry) (gethash score scoring-table))))
        (forward-line 1))
      ;; Get all the keys into a list.
      (maphash (lambda (score-key _cand-lst) (push score-key scoring-keys)) scoring-table)
      (setq scoring-keys (sort scoring-keys #'>))  ; Sort keys in order.
      (jcs--buffer-menu-clean)  ; Clean it.
      (dolist (key scoring-keys)
        (when (< jcs--buffer-menu--score-standard key)
          (let ((ens (sort (gethash key scoring-table)
                           (lambda (en1 en2)
                             (let ((en1-str (buffer-name (car en1)))
                                   (en2-str (buffer-name (car en2))))
                               (string-lessp en1-str en2-str))))))
            (dolist (en ens)
              (tabulated-list-print-entry (car en) (cdr en))))))
      (jcs-goto-line 2))
    (setq jcs--buffer-menu--done-filtering t)
    (jcs--safe-print-fake-header)
    ;; Once it is done filtering, we redo return action if needed.
    (when jcs--buffer-menu-return-delay
      (jcs-buffer-menu-return))))

(defun jcs--buffer-menu-trigger-filter ()
  "Trigger the filtering operation, with PRINT-HEADER."
  (tabulated-list-revert)
  (jcs--safe-print-fake-header)
  ;; NOTE: Ensure title exists.
  (when (> (length jcs--buffer-menu-search-title) (length tabulated-list--header-string))
    (setq-local tabulated-list--header-string jcs--buffer-menu-search-title))
  (setq jcs--buffer-menu--pattern (substring tabulated-list--header-string
                                             (length jcs--buffer-menu-search-title)
                                             (length tabulated-list--header-string)))
  (unless (string-empty-p jcs--buffer-menu--pattern)
    (setq jcs--buffer-menu--filter-timer (jcs-safe-kill-timer jcs--buffer-menu--filter-timer))
    (setq jcs--buffer-menu--done-filtering nil)
    (setq jcs--buffer-menu--filter-timer
          (run-with-idle-timer jcs--buffer-menu--filter-delay
                               nil
                               'jcs--buffer-menu-filter-list))))

(defun jcs--buffer-menu-input (key-input &optional add-del-num)
  "Insert key KEY-INPUT for fake header for search bar.
ADD-DEL-NUM : Addition or deletion number."
  (setq jcs--buffer-menu--fake-header-already-appears t)
  (unless add-del-num (setq add-del-num (length key-input)))
  (if (jcs-is-positive add-del-num)
      (setq tabulated-list--header-string
            (concat tabulated-list--header-string key-input))
    (setq tabulated-list--header-string
          (substring tabulated-list--header-string 0 (1- (length tabulated-list--header-string)))))
  (jcs--buffer-menu-trigger-filter))


(provide 'jcs-buffer-menu)
;;; jcs-buffer-menu.el ends here
