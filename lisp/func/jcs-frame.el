;;; jcs-frame.el --- Frame related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(jcs-add-hook 'after-make-frame-functions
  (jcs-theme-refresh)
  (select-frame frame)
  ;; split the winodw after create the new window
  (split-window-horizontally))

(defun jcs-frame-util-p (&optional frame)
  "Return non-nil if FRAME is an utility frame."
  (unless frame (setq frame (selected-frame)))
  (frame-parent frame))

(defun jcs-is-frame-maximize-p ()
  "Return nil, if frame is not maximized."
  (require 'asoc)
  (asoc-get (frame-parameters) 'fullscreen))

(defun jcs-make-frame ()
  "Select new frame after make frame."
  (interactive)
  (let ((new-frame (call-interactively #'make-frame)))
    (select-frame-set-input-focus new-frame)))

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

(defun jcs-max-frame-width ()
  "Find the largest frame width."
  (let ((fw (frame-width)))
    (dolist (fm (frame-list))
      (when (< fw (frame-width fm))
        (setq fw (frame-width fm))))
    fw))

(provide 'jcs-frame)
;;; jcs-frame.el ends here
