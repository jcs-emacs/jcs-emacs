;;; jcs-buffer-menu.el --- Function in buffer menu mode. (*Buffer List*).  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-refresh-buffer-menu-buffer ()
  "Update buffer menu buffer."
  (interactive)
  (save-selected-window
    (when (ignore-errors (jcs-jump-shown-to-buffer "*Buffer List*"))
      (jcs-buffer-menu))))

;; TOPIC: BufferMenuPlus
;; URL: https://www.emacswiki.org/emacs/BufferMenuPlus
;; Sorted by (1) visit, (2) buffer, (3) size, (4) time, (5) mode, (6) file. More

;;;###autoload
(defun jcs-buffer-menu-sort-by-visit ()
  "Sort the Buffer Menu List by visit."
  (interactive)
  (Buffer-menu-sort 1)
  (goto-char (point-min)))

;;;###autoload
(defun jcs-buffer-menu-sort-by-buffer ()
  "Sort the Buffer Menu List by buffer."
  (interactive)
  (Buffer-menu-sort 2)
  (goto-char (point-min)))

;;;###autoload
(defun jcs-buffer-menu-sort-by-size ()
  "Sort the Buffer Menu List by size."
  (interactive)
  (Buffer-menu-sort 3)
  (goto-char (point-min)))

;;;###autoload
(defun jcs-buffer-menu-sort-by-time ()
  "Sort the Buffer Menu List by time."
  (interactive)
  (Buffer-menu-sort 4)
  (goto-char (point-min)))

;;;###autoload
(defun jcs-buffer-menu-sort-by-mode ()
  "Sort the Buffer Menu List by mode."
  (interactive)
  (Buffer-menu-sort 5)
  (goto-char (point-min)))

;;;###autoload
(defun jcs-buffer-menu-sort-by-file ()
  "Sort the Buffer Menu List by file name."
  (interactive)
  (Buffer-menu-sort 6)
  (goto-char (point-min)))


(provide 'jcs-buffer-menu)
;;; jcs-buffer-menu.el ends here
