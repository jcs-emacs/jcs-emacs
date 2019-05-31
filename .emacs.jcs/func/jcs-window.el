;;; jcs-window.el --- Window related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'ace-window)
(require 'windmove)

;;-----------------------------------------------------------
;; Navigation
;;-----------------------------------------------------------

(defun jcs-ensure-switch-to-buffer-other-window (win-name)
  "Ensure switch to buffer, try multiple times.
Because sometime first time switching the buffer would not success."
  (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
    (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
      (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
        (switch-to-buffer-other-window win-name)))))

;;;###autoload
(defun jcs-jump-shown-to-buffer (in-buffer-name)
  "Jump to the buffer if the buffer current shown in the window.
If there is two window shown the same buffer/file, then it will
choose the one which is close to the next buffer.
IN-BUFFER-NAME : taget buffer name to jump to."
  (interactive "bEnter buffer to jump to: ")
  (let ((win-len (jcs-count-windows))
        (index 0)
        (found nil))
    (while (< index win-len)
      ;; NOTE(jenchieh): we use `string-match-p' instead
      ;; of `string=' because some buffer cannot be detected
      ;; in the buffer list. For instance, `*undo-tree*' is
      ;; buffer that cannot be detected for some reason.
      (if (string-match-p (buffer-name) in-buffer-name)
          (setq found t)
        (jcs-other-window-next))
      (setq index (1+ index)))

    ;; If not found, prompt error.
    (unless found
      (error "'%s' does not shown in any window" in-buffer-name))
    ;; Nothing happend return the value.
    found))

;;;###autoload
(defun jcs-switch-to-previous-buffer (&optional cnt)
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers.
CNT : Count for buffer to switch to."
  (interactive)
  (let (;; Default is 1.
        (target-cnt 1))
    (when cnt
      (setq target-cnt cnt))
    (switch-to-buffer (other-buffer (current-buffer) target-cnt))))

;;;###autoload
(defun jcs-switch-to-next-buffer-not-nil ()
  "Switch to the previous buffer that are not nil."
  (interactive)
  (when (<= 1 (jcs-not-nil-buffer-count))
    (let ((found-not-nil-buf nil))
      (while (not found-not-nil-buf)
        (call-interactively #'switch-to-next-buffer)
        (when (buffer-file-name)
          (setq found-not-nil-buf t))))))

;;;###autoload
(defun jcs-switch-to-prev-buffer-not-nil ()
  "Switch to the previous buffer that are not nil."
  (interactive)
  (when (<= 1 (jcs-not-nil-buffer-count))
    (let ((found-not-nil-buf nil))
      (while (not found-not-nil-buf)
        (call-interactively #'switch-to-prev-buffer)
        (when (buffer-file-name)
          (setq found-not-nil-buf t))))))

(defun jcs-not-nil-buffer-count ()
  "Returns count of the not nil buffer."
  (save-window-excursion
    (let ((index 0)
          (buf-len (length (buffer-list)))
          (buf-list '()))
      (while (< index buf-len)
        (when (buffer-file-name)
          (push (buffer-file-name) buf-list))
        (call-interactively #'switch-to-next-buffer)
        (setq index (+ index 1)))
      (setq buf-list (remove-duplicates buf-list))
      (length buf-list))))

(defun jcs-buffer-in-window-list ()
  "Get all the buffer in window list."
  ;; TOPIC(jenchieh): Show all open buffers in Emacs
  ;; SOURCE(jenchieh): http://stackoverflow.com/questions/12186713/show-all-open-buffers-in-emacs
  (let (buffers)
    (walk-windows
     (lambda (window)
       (push (window-buffer window) buffers)) t t)
    buffers))

(defun jcs-count-windows ()
  "Total window count."
  (save-selected-window
    (let ((count 0)
          (frame-len (length (frame-list)))
          (current-frame-count 0)
          (frame-counter 0))
      (while (< frame-counter frame-len)
        (setq current-frame-count (count-windows))
        (setq count (+ count current-frame-count))
        (let ((index 0))
          (while (< index current-frame-count)
            (call-interactively #'jcs-other-window-next)
            (setq index (+ index 1))))
        (setq frame-counter (+ frame-counter 1)))
      count)))

(defun jcs-buffer-visible-list ()
  "List of buffer that current visible in frame."
  (save-selected-window
    (let ((win-len (jcs-count-windows))
          (index 0)
          (buffers '()))
      (while (> win-len index)
        (push (buffer-name) buffers)
        (jcs-other-window-next)
        (setq index (+ index 1)))
      buffers)))

(defun jcs-buffer-showns (in-buf-name)
  "Check if buffer showns how many times?
IN-BUF-NAME : input buffer name you want to check.
Returns the count of the buffer shown in the window.
If nout found, returns 0."
  (let ((displayed-frame-count 0))
    (dolist (buf (jcs-buffer-visible-list))
      (when (string= buf in-buf-name)
        (setq displayed-frame-count (+ displayed-frame-count 1))))
    displayed-frame-count))

;;;###autoload
(defun jcs-walk-through-all-windows-once (&optional fnc)
  "Walk through all the windows once.
FNC : Callback apply to each windows."
  (interactive)
  (save-selected-window
    (let ((index 0))
      (while (< index (jcs-count-windows))
        (when fnc
          (funcall fnc))
        (jcs-other-window-next)
        (setq index (+ index 1))))))

;;-----------------------------------------------------------
;; Deleting
;;-----------------------------------------------------------

;;;###autoload
(defun jcs-balance-delete-window ()
  "Balance windows after deleting a window."
  (interactive)
  (delete-window)
  (balance-windows))

;;;###autoload
(defun jcs-balance-split-window-horizontally ()
  "Balance windows after split window horizontally."
  (interactive)
  (split-window-horizontally)
  (balance-windows))

;;-----------------------------------------------------------
;; Splitting
;;-----------------------------------------------------------

(defvar jcs-is-enlarge-buffer nil
  "Is any buffer in the frame enlarge already?")

(defvar-local jcs-is-enlarge-current-buffer nil
  "Is the current buffer enlarge already?")

;;;###autoload
(defun jcs-toggle-enlarge-window-selected ()
  "Toggle between show the whole buffer and current window state."
  (interactive)
  (if (and jcs-is-enlarge-current-buffer
           jcs-is-enlarge-buffer)
      (progn
        (balance-windows)
        (setq jcs-is-enlarge-buffer nil))
    (progn
      (maximize-window)

      ;; Set all local enlarge to false.
      (jcs-setq-all-local-buffer 'jcs-is-enlarge-current-buffer
                                 nil)

      ;; Current buffer is enlarge.
      (setq-local jcs-is-enlarge-current-buffer t)

      ;; One buffer in the frame is enlarge.
      (setq jcs-is-enlarge-buffer t))))


;;;###autoload
(defun jcs-toggle-window-split-hv ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (save-selected-window
    (let ((win-len (count-windows))
          (windmove-wrap-around nil))
      (if (= win-len 2)
          (progn
            (let ((other-win-buf nil)
                  (split-h-now t)
                  (window-switched nil))

              (when (or (window-in-direction 'above)
                        (window-in-direction 'below))
                (setq split-h-now nil))

              (if split-h-now
                  (progn
                    (when (window-in-direction 'right)
                      (windmove-right 1)
                      (setq window-switched t)))
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
              (when window-switched
                (other-window 1))))
        (error "Cannot toggle vertical/horizontal editor layout with more than 2 window in current frame")))))


;;-----------------------------------------------------------
;; Util
;;-----------------------------------------------------------

(defun jcs-window-is-larger-in-height-p ()
  "Get the window that are larget than other windows in vertical.
If non-nil, current window's height is larger than neighbor windows.
If nil, current window's height is smaller than neighbor windows."
  (let ((is-larger nil)
        (cur-win-h (window-height))
        (next-win-h -1)
        (prev-win-h -1))
    (if (window-full-height-p)
        (setq is-larger t)
      (progn
        (save-selected-window
          (jcs-other-window-next)
          (setq next-win-h (window-height)))
        (save-selected-window
          (jcs-other-window-prev)
          (setq prev-win-h (window-height)))
        (when (or (>= cur-win-h prev-win-h)
                  (>= cur-win-h next-win-h))
          (setq is-larger t))))
    is-larger))

;;;###autoload
(defun jcs-move-to-upmost-window (&optional not-all-frame)
  "Move to the upmost window.
NOT-ALL-FRAME : Default boundaries is all frame, limit to curent frame."
  (interactive)
  (if not-all-frame
      (let ((windmove-wrap-around nil))
        (ignore-errors
          (windmove-up jcs-windmove-max-move-count)))
    (jcs-ace-window-min)))

;;;###autoload
(defun jcs-move-to-downmost-window (&optional not-all-frame)
  "Move to the downmost window.
NOT-ALL-FRAME : Default boundaries is all frame, limit to curent frame."
  (interactive)
  (if not-all-frame
      (let ((windmove-wrap-around nil))
        (ignore-errors
          (windmove-down jcs-windmove-max-move-count)))
    (jcs-ace-window-max)))

;;;###autoload
(defun jcs-move-to-leftmost-window (&optional not-all-frame)
  "Move to the leftmost window.
NOT-ALL-FRAME : Default boundaries is all frame, limit to curent frame."
  (interactive)
  (if not-all-frame
      (let ((windmove-wrap-around nil))
        (ignore-errors
          (windmove-left jcs-windmove-max-move-count)))
    (jcs-ace-window-min)))

;;;###autoload
(defun jcs-move-to-rightmost-window (&optional not-all-frame)
  "Move to the rightmost window.
NOT-ALL-FRAME : Default boundaries is all frame, limit to curent frame."
  (interactive)
  (if not-all-frame
      (let ((windmove-wrap-around nil))
        (ignore-errors
          (windmove-right jcs-windmove-max-move-count)))
    (jcs-ace-window-max)))


;;-----------------------------------------------------------
;; Transparent
;;-----------------------------------------------------------

(defvar jcs-current-frame-transparency 100
  "Current active frame transparency.")

(defvar jcs-record-toggle-frame-transparency 85
  "Record toggle frame transparency.")

(defvar jcs-default-delta-transparency 5
  "Delta increament/decreament transparency value.")


;;;###autoload
(defun jcs-ask-set-transparency (alpha-level)
  "Set the frame transparency.
ALPHA-LEVEL : Target alpha level you want to set to the current frame."
  (interactive "p")
  ;; SOURCE: https://gist.github.com/benzap/89759928060f4578c063
  (let ((alpha-level (if (< alpha-level 2)
                         (read-number "Opacity percentage: " jcs-record-toggle-frame-transparency)
                       alpha-level)))
    (jcs-set-transparency alpha-level)))

(defun jcs-set-transparency (alpha-level)
  "Set the frame transparency.
ALPHA-LEVEL : Target alpha level you want to set to the current frame."
  (set-frame-parameter nil 'alpha alpha-level)
  (message (format "Frame alpha level is %d" (frame-parameter nil 'alpha)))
  (setq jcs-current-frame-transparency alpha-level)
  (unless (= alpha-level 100)
    (setq jcs-record-toggle-frame-transparency alpha-level)))

;;;###autoload
(defun jcs-toggle-transparent-frame ()
  "Toggle frame's transparency between `recorded'% and 100%."
  (interactive)
  (if (= jcs-current-frame-transparency 100)
      (jcs-set-transparency jcs-record-toggle-frame-transparency)
    (jcs-set-transparency 100)))


(defun jcs-delta-frame-transparent (del-trans)
  "Delta change the frame transparency by a certain percentage.
DEL-TRANS : Delta transparency value."
  (let ((alpha (frame-parameter nil 'alpha))
        (current-transparency jcs-default-delta-transparency))

    (setq current-transparency (cond ((numberp alpha) alpha)
                                     ((numberp (cdr alpha)) (cdr alpha))
                                     ;; Also handle undocumented (<active> <inactive>) form.
                                     ((numberp (cadr alpha)) (cadr alpha))))

    (setq current-transparency (+ current-transparency del-trans))
    (setq current-transparency (jcs-clamp-integer current-transparency 5 100))

    ;; Apply the value to frame.
    (jcs-set-transparency current-transparency)))

;;;###autoload
(defun jcs-increment-frame-transparent (&optional del-trans)
  "Increment the frame transparency by a certain percentage.
DEL-TRANS : Delta transparency value."
  (interactive)
  (unless del-trans
    (setq del-trans (jcs-to-positive jcs-default-delta-transparency)))
  (jcs-delta-frame-transparent del-trans))

;;;###autoload
(defun jcs-decrement-frame-transparent (&optional del-trans)
  "Decrement the frame transparency by a certain percentage.
DEL-TRANS : Delta transparency value."
  (interactive)
  (unless del-trans
    (setq del-trans (jcs-to-negative jcs-default-delta-transparency)))
  (jcs-delta-frame-transparent del-trans))


;;-----------------------------------------------------------
;; Get Window
;;-----------------------------------------------------------

(defun jcs-current-window-id ()
  "Return the current window id."
  (save-selected-window
    (let ((win-id -1)
          (cur-wind (selected-window))
          (index 0))
      (jcs-ace-window-min)
      (jcs-walk-through-all-windows-once
       (lambda ()
         (when (eq cur-wind (selected-window))
           (setq win-id index))
         (setq index (1+ index))))
      win-id)))

(defun jcs-get-window-id-by-buffer-name (buf-name)
  "Return a list of window id if match the buffer name.
BUF-NAME : Buffer name to search with."
  (save-selected-window
    (let ((win-id-lst '())
          (index 0))
      (jcs-ace-window-min)
      (jcs-walk-through-all-windows-once
       (lambda ()
         (when (string= buf-name (jcs-buffer-name-or-buffer-file-name))
           (push index win-id-lst))
         (setq index (1+ index))))
      (setq win-id-lst (reverse win-id-lst))
      win-id-lst)))


;;-----------------------------------------------------------
;; Ace Window
;;-----------------------------------------------------------

(defun jcs-ace-select-window (win-id)
  "Use `ace-window' to select the window by using window index.
WIN-ID : Window index."
  (let ((wnd (nth win-id (aw-window-list))))
    (when wnd
      (select-window wnd)
      (select-frame-set-input-focus (selected-frame)))))

;;;###autoload
(defun jcs-ace-window-min ()
  "Select window min."
  (interactive)
  (jcs-ace-select-window 0))

;;;###autoload
(defun jcs-ace-window-max ()
  "Select window max."
  (interactive)
  (jcs-ace-select-window (1- (length (aw-window-list)))))

;;;###autoload
(defun jcs-ace-window-1 ()
  "Select window 1."
  (interactive)
  (jcs-ace-window-min))

;;;###autoload
(defun jcs-ace-window-2 ()
  "Select window 2."
  (interactive)
  (jcs-ace-select-window 1))

;;;###autoload
(defun jcs-ace-window-3 ()
  "Select window 3."
  (interactive)
  (jcs-ace-select-window 2))

;;;###autoload
(defun jcs-ace-window-4 ()
  "Select window 4."
  (interactive)
  (jcs-ace-select-window 3))

;;;###autoload
(defun jcs-ace-window-5 ()
  "Select window 5."
  (interactive)
  (jcs-ace-select-window 4))

;;;###autoload
(defun jcs-ace-window-6 ()
  "Select window 6."
  (interactive)
  (jcs-ace-select-window 5))

;;;###autoload
(defun jcs-ace-window-7 ()
  "Select window 7."
  (interactive)
  (jcs-ace-select-window 6))

;;;###autoload
(defun jcs-ace-window-8 ()
  "Select window 8."
  (interactive)
  (jcs-ace-select-window 7))

;;;###autoload
(defun jcs-ace-window-9 ()
  "Select window 9."
  (interactive)
  (jcs-ace-select-window 8))


(provide 'jcs-window)
;;; jcs-window.el ends here
