;;; jcs-frame.el --- Frame related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-aftermake-frame-functions-hook (frame)
  "Resetting the new frame just created."
  (jcs-refresh-theme)
  (select-frame frame)
  ;; split the winodw after create the new window
  (split-window-horizontally))
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
    (let ((cur-frame (selected-frame)) (index 0))
      (while (< index (length (frame-list)))
        (when fnc
          (funcall fnc))
        (call-interactively #'other-frame)
        (setq index (+ index 1)))
      (select-frame-set-input-focus cur-frame))))

(defun jcs-make-frame-simple (name width height fnc &rest args)
  "Make frame with a bunch of default variables set.
You will only have to fill in FNC, NAME, WIDTH and HEIGHT."
  (let ((doc-frame nil) (pixel-x nil) (pixel-y nil) (cp-buf nil)
        (abs-pixel-pos (window-absolute-pixel-position)))
    (setq pixel-x (car abs-pixel-pos))
    (setq pixel-y (+ (cdr abs-pixel-pos) (frame-char-height)))
    (setq doc-frame
          (make-frame (list (cons 'minibuffer nil)
                            (cons 'name name)
                            (cons 'width width)
                            (cons 'height height)
                            (cons 'visibility nil)
                            (cons 'parent-frame t)
                            (cons 'fullscreen nil)
                            (cons 'no-other-frame t)
                            (cons 'skip-taskbar t))))

    (with-selected-frame doc-frame
      ;; Force one window only.
      (while (not (= (length (window-list)) 1)) (delete-window))

      (apply fnc args)

      ;; Set x and y position.
      (set-frame-parameter nil 'left pixel-x)
      (set-frame-parameter nil 'top pixel-y))

    ;; Make frame visible again
    (make-frame-visible doc-frame)
    doc-frame))


(provide 'jcs-frame)
;;; jcs-frame.el ends here
