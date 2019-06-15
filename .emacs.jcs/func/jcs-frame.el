;;; jcs-frame.el --- Frame related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-aftermake-frame-functions-hook (frame)
  "Resetting the new frame just created."
  (jcs-refresh-theme)
  (select-frame frame)
  ;; split the winodw after create the new window
  (split-window-horizontally)
  )
(add-hook 'after-make-frame-functions 'jcs-aftermake-frame-functions-hook)

(defun jcs-is-frame-maximize-p ()
  "Return non-nil, if frame maximized.
Return nil, if frame not maximized."
  (cdr (assoc 'fullscreen (frame-parameters))))

;;;###autoload
(defun jcs-walk-through-all-frames-once (&optional fnc)
  "Walk through all the frames once.
FNC : Callback apply to each windows."
  (interactive)
  (save-selected-window
    (let ((index 0))
      (while (< index (length (frame-list)))
        (when fnc
          (funcall fnc))
        (call-interactively #'other-frame)
        (setq index (+ index 1))))))


(provide 'jcs-frame)
;;; jcs-frame.el ends here
