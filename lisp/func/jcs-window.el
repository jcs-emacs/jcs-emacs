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

(cl-defun jcs-safe-jump-shown-to-buffer (buffer &key success error type)
  "Safely jump to BUFFER's window and execute SUCCESS operations.

If BUFFER isn't showing; then execute ERROR operations instead.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (jcs-with-no-redisplay
    (if (jcs-buffer-shown-p buffer type)
        (save-selected-window
          (when (and success (jcs-jump-shown-to-buffer buffer t type))
            (funcall success)))
      (when error (funcall error)))))

(defun jcs-jump-shown-to-buffer (buffer &optional no-error type)
  "Jump to the BUFFER if the buffer current shown in the window.

If optional argument NO-ERROR is non-nil; then it won't trigger error.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (interactive "bEnter buffer to jump to: ")
  (let (found)
    (when (jcs-buffer-shown-p buffer type)
      (let ((win-len (jcs-count-windows)) (index 0))
        (while (and (< index win-len) (not found))
          (if (jcs-string-compare-p buffer (jcs-buffer-name-or-buffer-file-name) type)
              (setq found t)
            (other-window 1 t))
          (setq index (1+ index)))))
    ;; If not found, prompt error.
    (when (and (not found) (not no-error))
      (user-error "[ERROR] '%s' does not shown in any window" buffer))
    found))

(defun jcs-switch-to-buffer-other-window (buffer-or-name &optional norecord)
  "Same with function `switch-to-buffer-other-window' but also consider
larger window height in the calculation.

See function `switch-to-buffer-other-window' description for arguments
BUFFER-OR-NAME and NORECORD."
  (jcs-switch-to-next-window-larger-in-height)
  (pop-to-buffer-same-window buffer-or-name norecord))

(defun jcs-switch-to-previous-buffer (&optional cnt)
  "Switch to previously open buffer with CNT."
  (interactive)
  (let ((target-cnt 1))  ; Default is 1.
    (when cnt (setq target-cnt cnt))
    (switch-to-buffer (other-buffer (current-buffer) target-cnt))))

(defun jcs-switch-to-next-valid-buffer ()
  "Switch to the previous buffer that are not nil."
  (interactive)
  (when (jcs-valid-buffers-exists-p)
    (let* ((lst (jcs-valid-buffer-list))
           (target-index 1)
           (target-buffer (or (nth target-index lst) (nth 0 lst))))
      (switch-to-buffer target-buffer))))

(defun jcs-switch-to-prev-valid-buffer ()
  "Switch to the previous buffer that are not nil."
  (interactive)
  (when (jcs-valid-buffers-exists-p)
    (let* ((lst (jcs-valid-buffer-list))
           (target-index (1- (length lst)))
           (target-buffer (or (nth target-index lst) (nth 0 lst))))
      (switch-to-buffer target-buffer))))

(defun jcs-count-windows (&optional util)
  "Total windows count.

If optional argument UTIL is non-nil; it would count utility frame.
See function `jcs-frame-util-p' for the definition of utility frame."
  (let ((count 0))
    (dolist (fn (frame-list))
      (when (or util (not (jcs-frame-util-p fn)))
        (setq count (+ (length (window-list fn)) count))))
    count))

(defun jcs-buffer-visible-list ()
  "List of buffer that current visible in frame."
  (save-selected-window
    (let (buffers)
      (jcs-walk-windows (lambda () (push (buffer-name) buffers)) nil t)
      buffers)))

(defun jcs-buffer-shown-count (in-buf-name &optional type)
  "Return the count of the IN-BUF-NAME shown.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (let ((bv-lst (jcs-buffer-visible-list)) (cnt 0))
    (dolist (buf bv-lst)
      (when (jcs-string-compare-p in-buf-name buf type)
        (setq cnt (1+ cnt))))
    cnt))

(defun jcs-buffer-list-shown-p (buf-lst &optional type)
  "Return non-nil if BUF-LST shown in the program.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (cl-some (lambda (buf) (jcs-buffer-shown-p buf type)) buf-lst))

(defun jcs-buffer-shown-p (in-buf-name &optional type)
  "Return non-nil if IN-BUF-NAME shown in the program.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (>= (jcs-buffer-shown-count in-buf-name type) 1))

(defun jcs-buffer-shown-in-multiple-window-p (in-buf-name &optional type)
  "Check if IN-BUF-NAME shown in multiple windows.

For argument TYPE; see function `jcs-string-compare-p' for description."
  (>= (jcs-buffer-shown-count in-buf-name type) 2))

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
;; (@* "Ace Window" )
;;

(defun jcs-ace-select-window (win-id)
  "Use `ace-window' to select the window by using window index, WIN-ID."
  (require 'ace-window)
  (when-let ((wnd (nth win-id (aw-window-list))))
    (select-window wnd)
    (select-frame-set-input-focus (selected-frame))))

(defun jcs-ace-window-min () "Select window min." (interactive) (jcs-ace-select-window 0))
(defun jcs-ace-window-max () "Select window max." (interactive) (jcs-ace-select-window (1- (length (aw-window-list)))))

(defun jcs-ace-window-1 () "Select window 1." (interactive) (jcs-ace-window-min))
(defun jcs-ace-window-2 () "Select window 2." (interactive) (jcs-ace-select-window 1))
(defun jcs-ace-window-3 () "Select window 3." (interactive) (jcs-ace-select-window 2))
(defun jcs-ace-window-4 () "Select window 4." (interactive) (jcs-ace-select-window 3))
(defun jcs-ace-window-5 () "Select window 5." (interactive) (jcs-ace-select-window 4))
(defun jcs-ace-window-6 () "Select window 6." (interactive) (jcs-ace-select-window 5))
(defun jcs-ace-window-7 () "Select window 7." (interactive) (jcs-ace-select-window 6))
(defun jcs-ace-window-8 () "Select window 8." (interactive) (jcs-ace-select-window 7))
(defun jcs-ace-window-9 () "Select window 9." (interactive) (jcs-ace-select-window 8))

;;
;; (@* "Column" )
;;

(defun jcs-window-type-list-in-column (type)
  "Return the list of TYPE in column.
TYPE can be 'buffer or 'window."
  (let (type-list break windmove-wrap-around)
    (save-selected-window
      (jcs-move-to-upmost-window t)
      (while (not break)
        (push
         (cl-case type
           (`buffer (buffer-name))
           (`window (selected-window)))
         type-list)
        (setq break (not (ignore-errors (windmove-down))))))
    type-list))

(defun jcs-window-buffer-on-column-p (buf)
  "Check if BUF on same column."
  (jcs-contain-list-type-str buf (jcs-window-type-list-in-column 'buffer) 'regex t))

;;
;; (@* "Deleting" )
;;

(defun jcs-delete-window-downwind ()
  "Delete window in downwind order."
  (interactive)
  (other-window -1) (save-selected-window (other-window 1) (delete-window)))

;;
;; (@* "Splitting" )
;;

(defvar jcs-is-enlarge-buffer nil
  "Is any buffer in the frame enlarge already?")

(defvar-local jcs-is-enlarge-current-buffer nil
  "Is the current buffer enlarge already?")

(defun jcs-toggle-enlarge-window-selected ()
  "Toggle between show the whole buffer and current window state."
  (interactive)
  (if (and jcs-is-enlarge-current-buffer jcs-is-enlarge-buffer)
      (progn (balance-windows) (setq jcs-is-enlarge-buffer nil))
    (maximize-window)
    ;; Set all local enlarge to false
    (jcs-setq-all-local-buffer 'jcs-is-enlarge-current-buffer nil)
    ;; Current buffer is enlarge
    (setq-local jcs-is-enlarge-current-buffer t)
    ;; One buffer in the frame is enlarge
    (setq jcs-is-enlarge-buffer t)))

(defun jcs-toggle-window-split-hv ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (save-selected-window
    (let ((win-len (count-windows)) windmove-wrap-around)
      (if (= win-len 2)
          (let ((other-win-buf nil) (split-h-now t) (window-switched nil))
            (when (or (window-in-direction 'above) (window-in-direction 'below))
              (setq split-h-now nil))

            (if split-h-now
                (when (window-in-direction 'right)
                  (windmove-right 1)
                  (setq window-switched t))
              (when (window-in-direction 'below)
                (windmove-down 1)
                (setq window-switched t)))

            (setq other-win-buf (buffer-name))
            (call-interactively #'delete-window)

            (if split-h-now
                (call-interactively #'split-window-vertically)
              (call-interactively #'split-window-horizontally))
            (other-window 1)

            (switch-to-buffer other-win-buf)

            ;; If the window is switched, switch back to original window.
            (when window-switched (other-window 1)))
        (user-error "[WARNING] Can't toggle vertical/horizontal editor layout with more than 2 windows in current frame")))))

;;
;; (@* "Util" )
;;

(defun jcs-switch-to-next-window-larger-in-height ()
  "Switch to next larger window in current column."
  (let ((current-window (selected-window)) larger-window win)
    (jcs-walk-windows
     (lambda ()
       (setq win (selected-window))
       (when (and (not larger-window) (not (eq current-window win))
                  (jcs-window-is-larger-in-height-p win))
         (setq larger-window win))))
    (select-window larger-window)))

(defun jcs-window-is-larger-in-height-p (&optional window)
  "Get the window that are larget than other windows in vertical/column."
  (unless window (setq window (selected-window)))
  (with-selected-window window
    (let ((current-height (window-height)) (is-larger t))
      (dolist (win (jcs-window-type-list-in-column 'window))
        (when (> (window-height win) current-height)
          (setq is-larger nil)))
      is-larger)))

(defun jcs-move-to-upmost-window (&optional not-all-frame)
  "Move to the upmost window by flag NOT-ALL-FRAME."
  (interactive)
  (if not-all-frame
      (let (windmove-wrap-around) (while (ignore-errors (windmove-up))))
    (jcs-ace-window-min)))

(defun jcs-move-to-downmost-window (&optional not-all-frame)
  "Move to the downmost window by flag NOT-ALL-FRAME."
  (interactive)
  (if not-all-frame
      (let (windmove-wrap-around) (while (ignore-errors (windmove-down))))
    (jcs-ace-window-max)))

(defun jcs-move-to-leftmost-window (&optional not-all-frame)
  "Move to the leftmost window by flag NOT-ALL-FRAME."
  (interactive)
  (if not-all-frame
      (let (windmove-wrap-around) (while (ignore-errors (windmove-left))))
    (jcs-ace-window-min)))

(defun jcs-move-to-rightmost-window (&optional not-all-frame)
  "Move to the rightmost window by flag NOT-ALL-FRAME."
  (interactive)
  (if not-all-frame
      (let (windmove-wrap-around) (while (ignore-errors (windmove-right))))
    (jcs-ace-window-max)))

;;
;; (@* "Get Window" )
;;

(defun jcs-current-window-id ()
  "Return the current window id."
  (save-selected-window
    (let ((win-id -1) (cur-wind (selected-window)) (index 0))
      (jcs-ace-window-min)
      (jcs-walk-windows
       (lambda ()
         (when (eq cur-wind (selected-window)) (setq win-id index))
         (setq index (1+ index)))
       nil t)
      win-id)))

(defun jcs-get-window-id-by-buffer-name (buf-name)
  "Return a list of window id if match the BUF-NAME."
  (save-selected-window
    (let ((index 0) win-id-lst)
      (jcs-ace-window-min)
      (jcs-walk-windows
       (lambda ()
         (when (string= buf-name (jcs-buffer-name-or-buffer-file-name))
           (push index win-id-lst))
         (setq index (1+ index)))
       nil t)
      (setq win-id-lst (reverse win-id-lst))
      win-id-lst)))

;;
;; (@* "Restore Windows Status" )
;;

(defvar jcs-window--record-buffer-names nil "Record all windows' buffer.")
(defvar jcs-window--record-points nil "Record all windows' point.")
(defvar jcs-window--record-first-visible-lines nil
  "Record all windows' first visible line.")

(defun jcs-window-record-once ()
  "Record windows status once."
  (let (buf-names pts f-lns)
    ;; Record down all the window information with the same buffer opened.
    (jcs-walk-windows
     (lambda ()
       (push (jcs-buffer-name-or-buffer-file-name) buf-names)  ; Record as string!
       (push (point) pts)
       (push (jcs-first-visible-line-in-window) f-lns))
     nil t)
    ;; Reverse the order to have the information order corresponding to the window
    ;; order correctly.
    (setq buf-names (reverse buf-names) pts (reverse pts) f-lns (reverse f-lns))
    (push buf-names jcs-window--record-buffer-names)
    (push pts jcs-window--record-points)
    (push f-lns jcs-window--record-first-visible-lines)))

(defun jcs-window-restore-once ()
  "Restore windows status once."
  (let* ((buf-names (pop jcs-window--record-buffer-names)) (pts (pop jcs-window--record-points))
         (f-lns (pop jcs-window--record-first-visible-lines))
         (win-cnt 0) buf-name (current-pt -1) (current-first-vs-line -1)
         actual-buf)
    ;; Restore the window information after, including opening the same buffer.
    (jcs-walk-windows
     (lambda ()
       (setq buf-name (nth win-cnt buf-names)
             current-pt (nth win-cnt f-lns)
             current-first-vs-line (nth win-cnt pts)
             actual-buf (jcs-get-buffer-by-path buf-name))
       (if actual-buf (switch-to-buffer actual-buf) (find-file buf-name))
       (jcs-make-first-visible-line-to current-pt)
       (goto-char current-first-vs-line)
       (setq win-cnt (1+ win-cnt)))
     nil t)))

(provide 'jcs-window)
;;; jcs-window.el ends here
