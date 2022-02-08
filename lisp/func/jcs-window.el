;;; jcs-window.el --- Window related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(defun jcs-walk-frames (fun &optional minibuf)
  "Like `walk-windows', but only for frames.

See function `walk-windows' description for arguments FUN and MINIBUF."
  (let (last-frame cur-frame)
    (walk-windows
     (lambda (win)
       (setq cur-frame (window-frame win))
       (unless (equal last-frame cur-frame)
         (setq last-frame cur-frame)
         (with-selected-frame cur-frame (funcall fun))))
     minibuf t)))

;;
;; (@* "Navigation" )
;;

(cl-defun jcs-jump-to-buffer-windows (buffer &key success error type)
  "Safely jump to BUFFER's window and execute SUCCESS operations.

If BUFFER isn't showing; then execute ERROR operations instead.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (jcs-with-no-redisplay
    (if (jcs-buffer-shown-p buffer type)
        (jcs-walk-windows
         (lambda ()
           (when (and success (jcs-string-compare-p buffer (jcs-buffer-name-or-buffer-file-name) type))
             (funcall success))))
      (when error (funcall error)))))

(defun jcs-switch-to-buffer-other-window (buffer-or-name &optional norecord)
  "Same with function `switch-to-buffer-other-window' but also consider
larger window height in the calculation.

See function `switch-to-buffer-other-window' description for arguments
BUFFER-OR-NAME and NORECORD."
  (select-window (get-largest-window nil nil t))
  (pop-to-buffer-same-window buffer-or-name norecord))

(defun jcs-switch-to-previous-buffer (&optional cnt)
  "Switch to previously open buffer with CNT."
  (interactive)
  (let ((cnt (or cnt 1)))  ; Default is 1.
    (switch-to-buffer (other-buffer (current-buffer) cnt))))

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

(defun jcs-buffer-visible-list ()
  "List of buffer that current visible in frame."
  (let (buffers)
    (dolist (win (window-list))
      (push (buffer-name (window-buffer win)) buffers))
    buffers))

(defun jcs-buffer-shown-count (buf-name &optional type)
  "Return the count of the IN-BUF-NAME shown.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (let ((cnt 0))
    (dolist (buf (jcs-buffer-visible-list))
      (when (jcs-string-compare-p buf-name buf type)
        (cl-incf cnt)))
    cnt))

(defun jcs-buffer-list-shown-p (buf-lst &optional type)
  "Return non-nil if BUF-LST shown in the program.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (cl-some (lambda (buf) (jcs-buffer-shown-p buf type)) buf-lst))

(defun jcs-buffer-shown-p (buf-name &optional type)
  "Return non-nil if IN-BUF-NAME shown in the program.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (>= (jcs-buffer-shown-count buf-name type) 1))

(defun jcs-buffer-shown-in-multiple-window-p (buf-name &optional type)
  "Check if IN-BUF-NAME shown in multiple windows.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (>= (jcs-buffer-shown-count buf-name type) 2))

(defun jcs-walk-windows (fun &optional minibuf all-frames)
  "See function `walk-windows' description for arguments FUN, MINIBUF and
ALL-FRAMES."
  (jcs-with-no-redisplay
    (walk-windows
     (lambda (win)
       (unless (jcs-frame-util-p (window-frame win))
         (with-selected-window win (funcall fun))))
     minibuf all-frames)))

;;
;; (@* "Deleting" )
;;

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
(defvar jcs-window--record-line-numbers nil "Record all windows' line numbers.")
(defvar jcs-window--record-columns nil "Record all windows' column.")
(defvar jcs-window--record-first-visible-lines nil
  "Record all windows' first visible line.")

(defun jcs-window-record-once ()
  "Record windows status once."
  (let (buf-names lns cols f-lns)
    ;; Record down all the window information with the same buffer opened.
    (jcs-walk-windows
     (lambda ()
       (push (jcs-buffer-name-or-buffer-file-name) buf-names)  ; Record as string!
       (push (line-number-at-pos) lns)
       (push (current-column) cols)
       (push (jcs-first-visible-line-in-window) f-lns)))
    ;; Reverse the order to have the information order corresponding to the window
    ;; order correctly.
    (setq buf-names (reverse buf-names) f-lns (reverse f-lns)
          lns (reverse lns) cols (reverse cols))
    (push buf-names jcs-window--record-buffer-names)
    (push lns jcs-window--record-line-numbers)
    (push cols jcs-window--record-columns)
    (push f-lns jcs-window--record-first-visible-lines)))

(defun jcs-window-restore-once ()
  "Restore windows status once."
  (let ((buf-names (pop jcs-window--record-buffer-names))
        (lns (pop jcs-window--record-line-numbers))
        (cols (pop jcs-window--record-columns))
        (f-lns (pop jcs-window--record-first-visible-lines))
        (win-cnt 0))
    ;; Restore the window information after, including opening the same buffer.
    (jcs-walk-windows
     (lambda ()
       (let* ((buf-name (nth win-cnt buf-names))
              (current-ln (nth win-cnt lns))
              (current-col (nth win-cnt cols))
              (current-first-vs-line (nth win-cnt f-lns))
              (actual-buf (jcs-get-buffer-by-path buf-name)))
         (if actual-buf (switch-to-buffer actual-buf) (find-file buf-name))
         (jcs-make-first-visible-line-to current-first-vs-line)
         (jcs-goto-line current-ln)
         (move-to-column current-col)
         (cl-incf win-cnt))))))

(provide 'jcs-window)
;;; jcs-window.el ends here
