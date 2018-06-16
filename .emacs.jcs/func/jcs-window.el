;; ========================================================================
;; $File: jcs-cc-func.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;---------------------------------------------
;; JenChieh Window Split Setting for Dual Monitor
;;---------------------------------------------

;;;###autoload
(defun jcs-new-window()
  "Setup dual monitor."
  (interactive)

  ;; open a new frame
  (make-frame-command))

;;;###autoload
(defun jcs-aftermake-frame-functions-hook (frame)
  "Resetting the new frame just created."
  (interactive)

  (select-frame frame)

  ;; split the winodw after create the new window
  (split-window-horizontally))
(add-hook 'after-make-frame-functions 'jcs-aftermake-frame-functions-hook)

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

;;;
;; URL(jenchieh): https://www.emacswiki.org/emacs/WindowNavigation
;; Author: ChrisDone
;;
(defun jcs-jump-shown-to-window (buffer-name)
  "Jump to window if the window is currently shown in the window.
BUFFER-NAME : buffer name."
  (interactive "bEnter buffer to jump to: ")
  (let ((visible-buffers (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list)))
        window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have visible window" buffer-name)
      (setq window-of-buffer
            (delq nil (mapcar '(lambda (window)
                                 (if (equal buffer-name (buffer-name (window-buffer window)))
                                     window nil)) (window-list))))
      (select-window (car window-of-buffer)))))

(defun jcs-jump-shown-to-buffer (in-buffer-name)
  "Jump to the buffer if the buffer current shown in the window.
If there is two window shown the same buffer/file, then it will
choose the one which is close to the next buffer. I think this
is the better version compare to `jcs-jump-shown-to-window' function."
  (interactive "bEnter buffer to jump to: ")
  (let ((win-len (length (window-list)))
        (index 0)
        (found nil))
    (while (< index win-len)
      ;; NOTE(jenchieh): we use `string-match-p' instead
      ;; of `string=' because some buffer cannot be detected
      ;; in the buffer list. For instance, `*undo-tree*' is
      ;; buffer that cannot be detected for some reason.
      (if (string-match-p in-buffer-name (buffer-name))
          (setq found t)
        (jcs-other-window-next))
      (setq index (1+ index)))

    ;; If not found, prompt error.
    (unless found
      (error "'%s' does not shown in any window" in-buffer-name))
    ;; Nothing happend return the value.
    found))

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
  (let ((delta-win-size 1000))
    (if (and jcs-is-enlarge-current-buffer
             jcs-is-enlarge-buffer)
        (progn
          (balance-windows)
          (setq jcs-is-enlarge-buffer nil))
      (progn
        (enlarge-window delta-win-size)               ;; Vertical enlarge
        (enlarge-window-horizontally delta-win-size)  ;; Horizontal enlarge

        ;; Set all local enlarge to false.
        (jcs-setq-all-local-buffer 'jcs-is-enlarge-current-buffer
                                   nil)

        ;; Current buffer is enlarge.
        (setq-local jcs-is-enlarge-current-buffer t)

        ;; One buffer in the frame is enlarge.
        (setq jcs-is-enlarge-buffer t)))))


(require 'windmove)

;;;###autoload
(defun jcs-toggle-window-split-hv ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)

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
