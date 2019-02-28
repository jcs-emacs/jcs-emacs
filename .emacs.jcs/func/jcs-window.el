;; ========================================================================
;; $File: jcs-cc-func.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-ensure-switch-to-buffer-other-window (win-name)
  "Ensure switch to buffer, try multiple times.
Because sometime first time switching the buffer would not success."
  (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
    (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
      (unless (or (ignore-errors (switch-to-buffer-other-window win-name)))
        (switch-to-buffer-other-window win-name)))))

;;-----------------------------------------------------------
;;-----------------------------------------------------------

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
  "Switch to the previous buffer that are not nil.
MAX-CNT : Maxinum count.
CNT : Counter."
  (interactive)
  (when (<= 1 (jcs-not-nil-buffer-count))
    (let ((found-not-nil-buf nil))
      (while (not found-not-nil-buf)
        (call-interactively #'switch-to-next-buffer)
        (when (buffer-file-name)
          (setq found-not-nil-buf t))))))

;;;###autoload
(defun jcs-switch-to-prev-buffer-not-nil (&optional cnt)
  "Switch to the previous buffer that are not nil.
MAX-CNT : Maxinum count.
CNT : Counter."
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

;;;###autoload
(defun jcs-visible-buffers (buffers)
  "given a list of buffers, return buffers which are currently
visible"
  (remove nil
          (mapcar
           '(lambda (buf)
              (if (get-buffer-window-list buf) buf))
           buffers)))

;;;###autoload
(defun jcs-not-visible-buffers (buffers)
  "given a list of buffers, return buffers which are not currently
visible"
  (remove nil
          (mapcar
           '(lambda (buf)
              (unless (get-buffer-window-list buf) buf))
           buffers)))

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

        (call-interactively #'jcs-other-window-next)

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

(defun jcs-in-window-list (buf)
  "Check if buffer open in window list.

buf : buffer name. (string)

True: return name.
False: return nil."
  (get-buffer-window-list buf))

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
      ;; Maximize the window
      (maximize-window)

      ;; Set all local enlarge to false.
      (jcs-setq-all-local-buffer 'jcs-is-enlarge-current-buffer
                                 nil)

      ;; Current buffer is enlarge.
      (setq-local jcs-is-enlarge-current-buffer t)

      ;; One buffer in the frame is enlarge.
      (setq jcs-is-enlarge-buffer t))))


(require 'windmove)

;;;###autoload
(defun jcs-toggle-window-split-hv ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (save-selected-window
    (let ((win-len (count-windows)))
      (if (= win-len 2)
          (progn
            ;; TEMPORARY(jenchieh): this is the temporary fixed, current
            ;; only works with 2 windows only.
            (jcs-toggle-window-split-hv-internal)
            (call-interactively #'jcs-other-window-next)
            (jcs-toggle-window-split-hv-internal)
            (call-interactively #'jcs-other-window-next))
        (error "Cannot toggle vertical/horizontal editor layout with more than 2 window in current frame")))))

(defun jcs-toggle-window-split-hv-internal ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  ;; TOPIC: Toggle Window Split
  ;; URL: https://www.emacswiki.org/emacs/ToggleWindowSplit
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))


;;-----------------------------------------------------------
;; Transparent
;;-----------------------------------------------------------

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

;;;###autoload
(defun jcs-set-transparency (alpha-level)
  "Set the frame transparency."
  ;; SOURCE: https://gist.github.com/benzap/89759928060f4578c063
  (interactive "p")
  (message (format "Alpha level passed in: %s" alpha-level))
  (let ((alpha-level (if (< alpha-level 2)
                         (read-number "Opacity percentage: " 85)
                       alpha-level))
        (myalpha (frame-parameter nil 'alpha)))
    (set-frame-parameter nil 'alpha alpha-level))
  (message (format "Alpha level is %d" (frame-parameter nil 'alpha))))

;;;###autoload
(defun jcs-toggle-transparency ()
  "Make the frame transparent."
  ;; SOURCE: https://www.emacswiki.org/emacs/TransparentEmacs
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         ;; NOTE(jenchieh): Second parameter is transparency
         ;; when not focus.
         ;; (when focus, when not focus)
         '(80 . 80) '(100 . 100)))))

(defvar jcs-default-delta-transparency 5
  "Delta increament/decreament transparency value.")

;;;###autoload
(defun jcs-increment-frame-transparent (&optional del-trans)
  "Increment the frame transparency by 5 percent."
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (setq current-transparency (cond ((numberp alpha) alpha)
                                     ((numberp (cdr alpha)) (cdr alpha))
                                     ;; Also handle undocumented (<active> <inactive>) form.
                                     ((numberp (cadr alpha)) (cadr alpha))))

    ;; Use default transparency value.
    (unless del-trans
      (setq del-trans jcs-default-delta-transparency))

    (setq current-transparency (+ current-transparency del-trans))
    (setq current-transparency (jcs-clamp-integer current-transparency 0 100))

    ;; Apply the value to frame.
    (jcs-set-transparency current-transparency)))

;;;###autoload
(defun jcs-decrement-frame-transparent (&optional del-trans)
  "Decrement the frame transparency by 5 percent."
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (setq current-transparency (cond ((numberp alpha) alpha)
                                     ((numberp (cdr alpha)) (cdr alpha))
                                     ;; Also handle undocumented (<active> <inactive>) form.
                                     ((numberp (cadr alpha)) (cadr alpha))))

    ;; Use default transparency value.
    (unless del-trans
      (setq del-trans jcs-default-delta-transparency))

    (setq current-transparency (- current-transparency del-trans))
    (setq current-transparency (jcs-clamp-integer current-transparency 0 100))

    ;; Apply the value to frame.
    (jcs-set-transparency current-transparency)))
