;;; jcs-window.el --- Window related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Window" )
;;

(defun jcs-get-largest-window (&optional all-frames dedicated not-selected no-other)
  "Like function `get-largest-window' but esure return a valid window."
  (or (get-largest-window all-frames dedicated not-selected no-other)
      (progn
        (split-window-sensibly)
        (get-largest-window all-frames dedicated not-selected no-other))))

;;
;; (@* "Frame" )
;;

(defun jcs-make-frame ()
  "Select new frame after make frame."
  (interactive)
  (let ((new-frame (call-interactively #'make-frame)))
    (select-frame-set-input-focus new-frame)
    (jcs-theme-refresh)
    (split-window-horizontally)))

;;
;; (@* "Navigation" )
;;

(cl-defun jcs-jump-to-buffer-windows (buffer &key success error type)
  "Safely jump to BUFFER's window and execute SUCCESS operations.

If BUFFER isn't showing; then execute ERROR operations instead.

For argument TYPE; see function `jcs-string-compare-p' description."
  (if-let* ((windows (jcs-window-list buffer type)))
      (dolist (win windows)
        (with-selected-window win
          (when success (funcall success))))
    (when error (funcall error))))

(defun jcs-switch-to-buffer-other-window (buffer-or-name &optional norecord)
  "Same with function `switch-to-buffer-other-window' but also consider
larger window height in the calculation.

See function `switch-to-buffer-other-window' description for arguments
BUFFER-OR-NAME and NORECORD."
  (select-window (jcs-get-largest-window nil nil t))
  (pop-to-buffer-same-window buffer-or-name norecord))

(defun jcs-switch-to-next-valid-buffer ()
  "Switch to the previous buffer that are not nil."
  (interactive)
  (when-let* ((lst (jcs-valid-buffer-list))
              (target-index 1)
              (target-buffer (or (nth target-index lst) (nth 0 lst))))
    (switch-to-buffer target-buffer)))

(defun jcs-switch-to-prev-valid-buffer ()
  "Switch to the previous buffer that are not nil."
  (interactive)
  (when-let* ((lst (jcs-valid-buffer-list))
              (target-index (1- (length lst)))
              (target-buffer (or (nth target-index lst) (nth 0 lst))))
    (switch-to-buffer target-buffer)))

(defun jcs-window-list (query &optional type ignore-case)
  "Return window list by it's QUERY.

For argument TYPE and IGNORE-CASE; see function `jcs-string-compare-p' description."
  (cl-remove-if-not
   (lambda (win)
     (jcs-string-compare-p query (buffer-name (window-buffer win)) type ignore-case))
   (window-list)))

(defun jcs-buffer-visible-list ()
  "List of buffer that current visible in frame."
  (mapcar (lambda (win) (buffer-name (window-buffer win))) (window-list)))

(defun jcs-buffer-shown-count (buf-name &optional type)
  "Return the count of the IN-BUF-NAME shown.

For argument TYPE; see function `jcs-string-compare-p' description."
  (length (jcs-window-list buf-name type)))

(defun jcs-buffer-list-shown-p (buf-lst &optional type)
  "Return non-nil if BUF-LST shown in the program.

For argument TYPE; see function `jcs-string-compare-p' description."
  (cl-some (lambda (buf) (jcs-buffer-shown-p buf type)) buf-lst))

(defun jcs-buffer-shown-p (buf-name &optional type)
  "Return non-nil if IN-BUF-NAME shown in the program.

For argument TYPE; see function `jcs-string-compare-p' description."
  (>= (jcs-buffer-shown-count buf-name type) 1))

(defun jcs-buffer-shown-in-multiple-window-p (buf-name &optional type)
  "Check if IN-BUF-NAME shown in multiple windows.

For argument TYPE; see function `jcs-string-compare-p' description."
  (>= (jcs-buffer-shown-count buf-name type) 2))

;;
;; (@* "Deleting" )
;;

(defun jcs-delete-window ()
  "Better UX of function `delete-window'."
  (interactive)
  (let ((next (or (window-in-direction 'above)
                  (window-in-direction 'below)
                  (window-in-direction 'right)
                  (window-in-direction 'left)
                  (window-prev-sibling (selected-window)))))
    (delete-window)
    (when next (select-window next))))

(defun jcs-delete-window-downwind ()
  "Delete window in downwind order."
  (interactive)
  (other-window -1) (save-selected-window (other-window 1) (delete-window)))

;;
;; (@* "Util" )
;;

(defun jcs-move-to-upmost-window ()
  "Move to the upmost window."
  (interactive)
  (while (ignore-errors (select-window (window-in-direction 'above)))))

(provide 'jcs-window)
;;; jcs-window.el ends here
