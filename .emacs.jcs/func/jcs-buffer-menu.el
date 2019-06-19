;;; jcs-buffer-menu.el --- Functions in buffer menu mode. (*Buffer List*).  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-buffer-menu-refresh-buffer ()
  "Update buffer menu buffer."
  (interactive)
  (save-selected-window
    (when (ignore-errors (jcs-jump-shown-to-buffer "*Buffer List*"))
      (buffer-menu))))

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


(provide 'jcs-buffer-menu)
;;; jcs-buffer-menu.el ends here
