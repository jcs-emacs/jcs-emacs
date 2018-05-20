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
(defun jcs-jump-to-window (buffer-name)
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

(defun jcs-jump-to-buffer (in-buffer-name)
  "Jump to the buffer if the buffer current shown in the window.
If there is two window shown the same buffer/file, then it will
choose the one which is close to the next buffer. I think this
is the better version compare to `jcs-jump-to-window' function."
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
