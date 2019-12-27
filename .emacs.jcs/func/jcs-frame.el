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
(defun jcs-make-frame ()
  "Select new frame after make frame."
  (interactive)
  (let ((new-frame (call-interactively #'make-frame)))
    (select-frame-set-input-focus new-frame)))

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

(defun jcs-max-frame-width ()
  "Find the largest frame width."
  (let ((fw (frame-width)))
    (dolist (fm (frame-list))
      (when (< fw (frame-width fm))
        (setq fw (frame-width fm))))
    fw))

(defun jcs-make-frame-simple (name x y width height fnc &rest)
  "Make frame with a bunch of default variables set.
You will only have to fill in NAME, X, Y, WIDTH, HEIGHT and FNC."
  (let ((doc-frame nil) (pixel-x x) (pixel-y y) (cp-buf nil)
        (abs-pixel-pos (window-absolute-pixel-position)))
    (unless pixel-x (setq pixel-x (car abs-pixel-pos)))
    (unless pixel-y (setq pixel-y (+ (cdr abs-pixel-pos) (frame-char-height))))

    (setq doc-frame
          (make-frame (list (cons 'parent-frame (window-frame))
                            (cons 'minibuffer nil)
                            (cons 'name name)
                            (cons 'width width)
                            (cons 'height height)
                            (cons 'visibility nil)
                            (cons 'parent-frame t)
                            (cons 'fullscreen nil)
                            (cons 'no-other-frame t)
                            (cons 'skip-taskbar t)
                            (cons 'vertical-scroll-bars nil)
                            (cons 'horizontal-scroll-bars nil)
                            (cons 'menu-bar-lines 0)
                            (cons 'tool-bar-lines 0)
                            (cons 'no-accept-focus t)
                            (cons 'no-special-glyphs t)
                            (cons 'no-other-frame t)
                            (cons 'cursor-type 'hollow)
                            ;; Do not save child-frame when use desktop.el
                            (cons 'desktop-dont-save t))))

    (with-selected-frame doc-frame
      ;; Force one window only.
      (while (not (= (length (window-list)) 1)) (delete-window))
      (when (functionp fnc) (funcall fnc)))

    ;; Set x and y position.
    (set-frame-parameter nil 'left pixel-x)
    (set-frame-parameter nil 'top pixel-y)

    ;; Make frame visible again
    (make-frame-visible doc-frame)
    doc-frame))

(defvar jcs-peek-frame nil
  "Record the peek frame, only allows one at a time.")

(defun jcs-create-peek-frame (name fnc)
  "Create the peek frame with NAME and FNC."
  (when jcs-peek-frame (delete-frame jcs-peek-frame))
  (let ((spawn-pt (window-absolute-pixel-position (jcs-column-to-point 0))))
    (setq jcs-peek-frame
          (jcs-make-frame-simple
           name
           (- (car spawn-pt) 4) nil
           (window-width) 15
           fnc))))


(provide 'jcs-frame)
;;; jcs-frame.el ends here
