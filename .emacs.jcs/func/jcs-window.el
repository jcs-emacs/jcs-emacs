;;; jcs-window.el --- Window related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Navigation

(defun jcs-ensure-switch-to-buffer-other-window (win-name)
  "Ensure switch to buffer, try multiple times with WIN-NAME"
  (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
    (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
      (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
        (switch-to-buffer-other-window win-name)))))

(defun jcs-safe-jump-shown-to-buffer (in-buffer-name &optional fnc exp-fnc)
  "Safely jump to IN-BUFFER-NAME's window and execute FNC with EXP-FNC."
  (save-selected-window
    (let ((fm (selected-frame)))
      (if (ignore-errors (jcs-jump-shown-to-buffer in-buffer-name))
          (when fnc (funcall fnc))
        (when exp-fnc (funcall exp-fnc)))
      (select-frame-set-input-focus fm))))

;;;###autoload
(defun jcs-jump-shown-to-buffer (in-buffer-name)
  "Jump to the IN-BUFFER-NAME if the buffer current shown in the window."
  (interactive "bEnter buffer to jump to: ")
  (let ((jcs--no-advice-other-window t)
        (win-len (jcs-count-windows)) (index 0) (found nil))
    (while (< index win-len)
      ;; NOTE: we use `string-match-p' instead of `string='
      ;; because some buffer cannot be detected in the buffer
      ;; list. For instance, `*undo-tree*' is buffer that cannot
      ;; be detected for some reason.
      (if (string-match-p in-buffer-name (jcs-buffer-name-or-buffer-file-name))
          (setq found t)
        (other-window 1 t))
      (setq index (1+ index)))
    ;; If not found, prompt error.
    (unless found
      (user-error "[ERROR] '%s' does not shown in any window" in-buffer-name))
    ;; Nothing happend return the value.
    found))

;;;###autoload
(defun jcs-switch-to-previous-buffer (&optional cnt)
  "Switch to previously open buffer with CNT."
  (interactive)
  (let ((target-cnt 1))  ; Default is 1.
    (when cnt
      (setq target-cnt cnt))
    (switch-to-buffer (other-buffer (current-buffer) target-cnt))))

;;;###autoload
(defun jcs-switch-to-next-buffer-not-nil ()
  "Switch to the previous buffer that are not nil."
  (interactive)
  (when (<= 1 (jcs-valid-buffers-in-buffer-list))
    (let ((found-not-nil-buf nil))
      (while (not found-not-nil-buf)
        (call-interactively #'switch-to-next-buffer)
        (when (buffer-file-name)
          (setq found-not-nil-buf t))))))

;;;###autoload
(defun jcs-switch-to-prev-buffer-not-nil ()
  "Switch to the previous buffer that are not nil."
  (interactive)
  (when (<= 1 (jcs-valid-buffers-in-buffer-list))
    (let ((found-not-nil-buf nil))
      (while (not found-not-nil-buf)
        (call-interactively #'switch-to-prev-buffer)
        (when (buffer-file-name)
          (setq found-not-nil-buf t))))))

(defun jcs-count-windows ()
  "Total window count."
  (let ((jcs--no-advice-other-window t)
        (count 0))
    (dolist (fn (frame-list))
      (setq count (+ (length (window-list fn)) count)))
    count))

(defun jcs-buffer-visible-list ()
  "List of buffer that current visible in frame."
  (save-selected-window
    (let ((jcs--no-advice-other-window t)
          (win-len (jcs-count-windows)) (index 0) (buffers '()))
      (while (> win-len index)
        (push (buffer-name) buffers)
        (other-window 1 t)
        (setq index (1+ index)))
      buffers)))

(defun jcs-buffer-showns (in-buf-name)
  "Check if IN-BUF-NAME showns in program."
  (let ((displayed-frame-count 0))
    (dolist (buf (jcs-buffer-visible-list))
      (when (string= buf in-buf-name)
        (setq displayed-frame-count (+ displayed-frame-count 1))))
    displayed-frame-count))

;;;###autoload
(defun jcs-walk-through-all-windows-once (&optional fnc do-advice)
  "Walk through all the windows once and execute callback FNC.
If DO-ADVICE is non-nil then will active advices from `other-window' function."
  (interactive)
  (let ((jcs--no-advice-other-window (if do-advice nil t)))
    (save-selected-window
      (let ((cur-frame (selected-frame)) (index 0))
        (while (< index (jcs-count-windows))
          (when fnc (funcall fnc))
          (other-window 1 t)
          (setq index (+ index 1)))
        (select-frame-set-input-focus cur-frame)))))

;;========================
;; Ace Window

(defun jcs-ace-select-window (win-id)
  "Use `ace-window' to select the window by using window index, WIN-ID."
  (require 'ace-window)
  (let ((wnd (nth win-id (aw-window-list))))
    (when wnd
      (select-window wnd)
      (select-frame-set-input-focus (selected-frame)))))

;;;###autoload
(defun jcs-ace-window-min () "Select window min." (interactive) (jcs-ace-select-window 0))
;;;###autoload
(defun jcs-ace-window-max () "Select window max." (interactive) (jcs-ace-select-window (1- (length (aw-window-list)))))

;;;###autoload
(defun jcs-ace-window-1 () "Select window 1." (interactive) (jcs-ace-window-min))
;;;###autoload
(defun jcs-ace-window-2 () "Select window 2." (interactive) (jcs-ace-select-window 1))
;;;###autoload
(defun jcs-ace-window-3 () "Select window 3." (interactive) (jcs-ace-select-window 2))
;;;###autoload
(defun jcs-ace-window-4 () "Select window 4." (interactive) (jcs-ace-select-window 3))
;;;###autoload
(defun jcs-ace-window-5 () "Select window 5." (interactive) (jcs-ace-select-window 4))
;;;###autoload
(defun jcs-ace-window-6 () "Select window 6." (interactive) (jcs-ace-select-window 5))
;;;###autoload
(defun jcs-ace-window-7 () "Select window 7." (interactive) (jcs-ace-select-window 6))
;;;###autoload
(defun jcs-ace-window-8 () "Select window 8." (interactive) (jcs-ace-select-window 7))
;;;###autoload
(defun jcs-ace-window-9 () "Select window 9." (interactive) (jcs-ace-select-window 8))

;;----------------------------------------------------------------------------
;; Advices

(defun jcs--delete-window--advice-after (&rest _)
  "Advice run after execute `delete-window' function."
  (jcs-buffer-menu-safe-refresh))
(advice-add 'delete-window :after #'jcs--delete-window--advice-after)

;;----------------------------------------------------------------------------
;; Column

(defun jcs-window-type-list-in-column (type)
  "Return the list of TYPE in column.
TYPE can be 'buffer or 'window."
  (let ((type-list '()) (break nil) (windmove-wrap-around nil))
    (save-selected-window
      (jcs-move-to-upmost-window t)
      (while (and (not break))
        (push
         (cl-case type
           ('buffer (buffer-name))
           ('window (selected-window)))
         type-list)
        (setq break (not (ignore-errors (windmove-down))))))
    type-list))

(defun jcs-window-buffer-on-column-p (buf)
  "Check if BUF on same column."
  (jcs-is-contain-list-string-regexp-reverse (jcs-window-type-list-in-column 'buffer) buf))

;;----------------------------------------------------------------------------
;; Deleting

;;;###autoload
(defun jcs-balance-delete-window ()
  "Balance windows after deleting a window."
  (interactive)
  (delete-window)
  (balance-windows))

;;;###autoload
(defun jcs-delete-window-downwind ()
  "Delete window in downwind order."
  (interactive)
  (other-window -1)
  (save-selected-window (other-window 1) (delete-window)))

;;----------------------------------------------------------------------------
;; Splitting

;;;###autoload
(defun jcs-balance-split-window-horizontally ()
  "Balance windows after split window horizontally."
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (save-selected-window
    (other-window 1)
    (jcs-bury-buffer)))

;;;###autoload
(defun jcs-balance-split-window-vertically ()
  "Balance windows after split window vertically."
  (interactive)
  (split-window-vertically)
  (balance-windows)
  (save-selected-window
    (other-window 1)
    (jcs-bury-buffer)))


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
    (maximize-window)

    ;; Set all local enlarge to false.
    (jcs-setq-all-local-buffer 'jcs-is-enlarge-current-buffer nil)

    ;; Current buffer is enlarge.
    (setq-local jcs-is-enlarge-current-buffer t)

    ;; One buffer in the frame is enlarge.
    (setq jcs-is-enlarge-buffer t)))


;;;###autoload
(defun jcs-toggle-window-split-hv ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (save-selected-window
    (let ((win-len (count-windows)) (windmove-wrap-around nil))
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

;;----------------------------------------------------------------------------
;; Util

(defun jcs-switch-to-next-window-larger-in-height ()
  "Return the next window that have larger height in column."
  (other-window 1 t)
  (let ((larger-window nil))
    (jcs-walk-through-all-windows-once
     (lambda ()
       (when (and (not larger-window)
                  (jcs-window-is-larger-in-height-p))
         (setq larger-window (selected-window)))))
    larger-window))

(defun jcs-window-is-larger-in-height-p ()
  "Get the window that are larget than other windows in vertical/column."
  (let ((jcs--no-advice-other-window t)
        (current-height (window-height)) (is-larger t))
    (dolist (win (jcs-window-type-list-in-column 'window))
      (when (> (window-height win) current-height)
        (setq is-larger nil)))
    is-larger))

;;;###autoload
(defun jcs-move-to-upmost-window (&optional not-all-frame)
  "Move to the upmost window by flag NOT-ALL-FRAME."
  (interactive)
  (if not-all-frame
      (let ((was-wrap-around windmove-wrap-around))
        (setq windmove-wrap-around nil)
        (while (ignore-errors (windmove-up)))
        (setq windmove-wrap-around was-wrap-around))
    (jcs-ace-window-min)))

;;;###autoload
(defun jcs-move-to-downmost-window (&optional not-all-frame)
  "Move to the downmost window by flag NOT-ALL-FRAME."
  (interactive)
  (if not-all-frame
      (let ((was-wrap-around windmove-wrap-around))
        (setq windmove-wrap-around nil)
        (while (ignore-errors (windmove-down)))
        (setq windmove-wrap-around was-wrap-around))
    (jcs-ace-window-max)))

;;;###autoload
(defun jcs-move-to-leftmost-window (&optional not-all-frame)
  "Move to the leftmost window by flag NOT-ALL-FRAME."
  (interactive)
  (if not-all-frame
      (let ((was-wrap-around windmove-wrap-around))
        (setq windmove-wrap-around nil)
        (while (ignore-errors (windmove-left)))
        (setq windmove-wrap-around was-wrap-around))
    (jcs-ace-window-min)))

;;;###autoload
(defun jcs-move-to-rightmost-window (&optional not-all-frame)
  "Move to the rightmost window by flag NOT-ALL-FRAME."
  (interactive)
  (if not-all-frame
      (let ((was-wrap-around windmove-wrap-around))
        (setq windmove-wrap-around nil)
        (while (ignore-errors (windmove-right)))
        (setq windmove-wrap-around was-wrap-around))
    (jcs-ace-window-max)))

;;----------------------------------------------------------------------------
;; Transparent

(defvar jcs-current-frame-transparency 100
  "Current active frame transparency.")

(defvar jcs-record-toggle-frame-transparency 85
  "Record toggle frame transparency.")

(defvar jcs-default-delta-transparency 5
  "Delta increament/decreament transparency value.")


;;;###autoload
(defun jcs-ask-set-transparency (alpha-level)
  "Set the frame transparency by ALPHA-LEVEL."
  (interactive "p")
  ;; SOURCE: https://gist.github.com/benzap/89759928060f4578c063
  (let ((alpha-level (if (< alpha-level 2)
                         (read-number "Opacity percentage: " jcs-record-toggle-frame-transparency)
                       alpha-level)))
    (jcs-set-transparency alpha-level)))

(defun jcs-set-transparency (alpha-level)
  "Set the frame transparency by ALPHA-LEVEL."
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
  "Delta changes the frame transparency by a certain percentage, DEL-TRANS."
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
  "Increment the frame transparency by a certain percentage, DEL-TRANS."
  (interactive)
  (unless del-trans
    (setq del-trans (jcs-to-positive jcs-default-delta-transparency)))
  (jcs-delta-frame-transparent del-trans))

;;;###autoload
(defun jcs-decrement-frame-transparent (&optional del-trans)
  "Decrement the frame transparency by a certain percentage, DEL-TRANS."
  (interactive)
  (unless del-trans
    (setq del-trans (jcs-to-negative jcs-default-delta-transparency)))
  (jcs-delta-frame-transparent del-trans))

;;----------------------------------------------------------------------------
;; Get Window

(defun jcs-current-window-id ()
  "Return the current window id."
  (save-selected-window
    (let ((win-id -1) (cur-wind (selected-window)) (index 0))
      (jcs-ace-window-min)
      (jcs-walk-through-all-windows-once
       (lambda ()
         (when (eq cur-wind (selected-window))
           (setq win-id index))
         (setq index (1+ index))))
      win-id)))

(defun jcs-get-window-id-by-buffer-name (buf-name)
  "Return a list of window id if match the BUF-NAME."
  (save-selected-window
    (let ((win-id-lst '()) (index 0))
      (jcs-ace-window-min)
      (jcs-walk-through-all-windows-once
       (lambda ()
         (when (string= buf-name (jcs-buffer-name-or-buffer-file-name))
           (push index win-id-lst))
         (setq index (1+ index))))
      (setq win-id-lst (reverse win-id-lst))
      win-id-lst)))

;;----------------------------------------------------------------------------
;; Center

;;;###autoload
(defun jcs-horizontal-recenter ()
  "Make the point horizontally centered in the window."
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

;;----------------------------------------------------------------------------
;; Restore Windows Status

(defvar jcs-window--record-buffers '() "Record all windows' buffer.")
(defvar jcs-window--record-points '() "Record all windows' point.")
(defvar jcs-window--record-first-visible-lines '()
  "Record all windows' first visible line.")

(defun jcs-window-record-once ()
  "Record windows status once."
  (setq jcs-window--record-points '())
  (setq jcs-window--record-buffers '())
  (setq jcs-window--record-first-visible-lines '())
  ;; Record down all the window information with the same
  ;; buffer opened.
  (jcs-walk-through-all-windows-once
   (lambda ()
     (push (jcs-buffer-name-or-buffer-file-name) jcs-window--record-buffers)
     (push (point) jcs-window--record-points)
     (push (jcs-first-visible-line-in-window) jcs-window--record-first-visible-lines)))
  ;; Reverse the order to have the information order corresponding
  ;; to the window order correctly.
  (setq jcs-window--record-buffers (reverse jcs-window--record-buffers))
  (setq jcs-window--record-points (reverse jcs-window--record-points))
  (setq jcs-window--record-first-visible-lines (reverse jcs-window--record-first-visible-lines)))

(defun jcs-window-restore-once ()
  "Restore windows status once."
  ;; Restore the window information after, including
  ;; opening the same buffer.
  (let ((win-cnt 0) (current-buf nil) (current-pt -1) (current-first-vs-line -1))
    (jcs-walk-through-all-windows-once
     (lambda ()
       (setq current-buf (nth win-cnt jcs-window--record-buffers))
       (setq current-pt (nth win-cnt jcs-window--record-first-visible-lines))
       (setq current-first-vs-line (nth win-cnt jcs-window--record-points))
       (if (get-buffer current-buf) (switch-to-buffer current-buf) (find-file current-buf))
       (jcs-make-first-visible-line-to current-pt)
       (goto-char current-first-vs-line)
       (setq win-cnt (1+ win-cnt))))))

(provide 'jcs-window)
;;; jcs-window.el ends here
