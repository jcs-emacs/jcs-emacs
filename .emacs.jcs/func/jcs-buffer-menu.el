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
  (if jcs-buffer-menu-done-filtering
      (if (ignore-errors (Buffer-menu-this-window))
          (message nil)  ; Use to clear `[Display not ready]'.
        (user-error "No buffer on this line"))
    (setq jcs-buffer-menu-return-delay t)
    (message "[Display not ready]")))


(defvar jcs-buffer-menu-done-filtering nil
  "Flag to check if done filtering.")

(defvar jcs-buffer-menu-filter-timer nil
  "Store filter timer function.")

(defvar jcs-buffer-menu-filter-delay 0.1
  "Filter delay time.")

(defun jcs-buffer-menu-fuzzy-match (pattern candidate)
  "Fuzzy match for searching buffer name."
  (unless (string-match " " pattern)
    (if (string-match "\\`!" pattern)
        (not (string-match pattern candidate))
      (string-match pattern candidate))))

(defun jcs-buffer-menu-filter-list ()
  "Do filtering the buffer list."
  (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
    (let* ((cl (string-trim (thing-at-point 'line)))
           (buf-name (elt (tabulated-list-get-entry) 3))
           (search-str (substring tabulated-list--header-string
                                  (length jcs-buffer-menu-search-title)
                                  (length tabulated-list--header-string))))
      (if (string-match-p search-str buf-name)
          (next-line)
        (tabulated-list-delete-entry))))
  (jcs-goto-line 2)
  (setq jcs-buffer-menu-done-filtering t)
  ;; Once it is done filtering, we redo return action if needed.
  (when jcs-buffer-menu-return-delay
    (jcs-buffer-menu-return)))

(defun jcs-buffer-menu-input (key-input &optional add-del-num)
  "Insert key KEY-INPUT for fake header for search bar.
ADD-DEL-NUM : Addition or deletion number."
  (setq jcs-buffer-menu-done-filtering nil)
  (unless add-del-num (setq add-del-num (length key-input)))
  (if (jcs-is-positive add-del-num)
      (setq tabulated-list--header-string
            (concat tabulated-list--header-string key-input))
    (setq tabulated-list--header-string
          (substring tabulated-list--header-string 0 (1- (length tabulated-list--header-string)))))
  ;; NOTE: Ensure title exists.
  (when (> (length jcs-buffer-menu-search-title) (length tabulated-list--header-string))
    (setq tabulated-list--header-string jcs-buffer-menu-search-title))
  (tabulated-list-revert)
  (tabulated-list-print-fake-header)
  (when (timerp jcs-buffer-menu-filter-timer)
    (cancel-timer jcs-buffer-menu-filter-timer))
  (setq jcs-buffer-menu-filter-timer
        (run-with-idle-timer jcs-buffer-menu-filter-delay
                             nil
                             'jcs-buffer-menu-filter-list)))


(provide 'jcs-buffer-menu)
;;; jcs-buffer-menu.el ends here
