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

(defun jcs-frame-util-p (&optional frame)
  "Return non-nil if FRAME is an utility frame."
  (frame-parent (or frame (selected-frame))))

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
  (if-let ((windows (jcs-window-list buffer type)))
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

(defun jcs-walk-windows (fun &optional minibuf all-frames)
  "See function `walk-windows' description for arguments FUN, MINIBUF and
ALL-FRAMES."
  (elenv-with-no-redisplay
    (walk-windows
     (lambda (win)
       (unless (jcs-frame-util-p (window-frame win))
         (with-selected-window win (funcall fun))))
     minibuf all-frames)))

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

;;
;; (@* "Restore Windows Status" )
;;

(defvar jcs-window--record-buffer-names nil "Record all windows' buffer.")
(defvar jcs-window--record-points nil "Record all windows point.")
(defvar jcs-window--record-first-visible-lines nil
  "Record all windows' first visible line.")

(defun jcs-window-record-once ()
  "Record windows status once."
  (let ((buf-names nil) (pts nil) (f-lns nil))
    ;; Record down all the window information with the same buffer opened.
    (jcs-walk-windows
     (lambda ()
       (jcs-push (jcs-buffer-name-or-buffer-file-name) buf-names)  ; Record as string!
       (jcs-push (point) pts)
       (jcs-push (jcs-first-visible-line-in-window) f-lns)))
    (push buf-names jcs-window--record-buffer-names)
    (push pts jcs-window--record-points)
    (push f-lns jcs-window--record-first-visible-lines)))

(defun jcs-window-restore-once ()
  "Restore windows status once."
  (let ((buf-names (pop jcs-window--record-buffer-names))
        (pts (pop jcs-window--record-points))
        (f-lns (pop jcs-window--record-first-visible-lines))
        (win-cnt 0))
    ;; Restore the window information after, including opening the same buffer.
    (jcs-walk-windows
     (lambda ()
       (let* ((buf-name (nth win-cnt buf-names))
              (current-pt (nth win-cnt pts))
              (current-first-vs-line (nth win-cnt f-lns))
              (actual-buf (jcs-get-buffer-by-path buf-name)))
         (if actual-buf (switch-to-buffer actual-buf) (find-file buf-name))
         (jcs-make-first-visible-line-to current-first-vs-line)
         (goto-char current-pt)
         (cl-incf win-cnt))))))

(provide 'jcs-window)
;;; jcs-window.el ends here
