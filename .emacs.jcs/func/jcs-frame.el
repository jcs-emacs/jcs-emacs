;;; jcs-frame.el --- Frame related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-new-frame ()
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

(defun jcs-is-frame-maximize-p ()
  "Return non-nil, if frame maximized.
Return nil, if frame not maximized."
  (cdr (assoc 'fullscreen (frame-parameters))))


(provide 'jcs-frame)
;;; jcs-frame.el ends here
