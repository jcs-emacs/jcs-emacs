;;; jcs-helm-func.el --- Helm function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; Util

(defun jcs--helm-find-window-line-height ()
  "How many lines in the current active helm window."
  (save-excursion
    (goto-char (point-max))
    (let ((vis-bot-line -1))
      (save-window-excursion
        (jcs-recenter-top-bottom 'bottom)
        (setq vis-bot-line (jcs-first-visible-line-in-window)))
      (- (line-number-at-pos) vis-bot-line))))

;;----------------------------------------------------------------------------
;; Core

(defun jcs-helm-before-initialize-hook ()
  "Do the helm `M-x' and change theme"
  ;; NOTE: Change theme so we know which mode we are in visually.
  (jcs-dark-blue-mode-line))
(add-hook 'helm-before-initialize-hook 'jcs-helm-before-initialize-hook)


;; TOPIC: How do I make pressing <RET> in helm-find-files open the directory?
;; SOURCE: http://emacs.stackexchange.com/questions/3798/how-do-i-make-pressing-ret-in-helm-find-files-open-the-directory

(defun jcs-helm-find-files-navigate-forward (orig-fun &rest args)
  "Advice run around `helm-execute-persistent-action' command."
  (if jcs-file--selecting-file
      (if (and (stringp (helm-get-selection))
               (file-directory-p (helm-get-selection)))
          )
    (if (and (equal "Find Files" (assoc-default 'name (helm-get-current-source)))
             (equal args nil)
             (stringp (helm-get-selection))
             (not (file-directory-p (helm-get-selection))))
        (progn
          (when jcs-helm-ff-other-window
            (setq jcs-helm-ff-other-window nil)
            (helm-ff-run-switch-other-window))
          (helm-maybe-exit-minibuffer))
      (apply orig-fun args))))
(advice-add 'helm-execute-persistent-action :around #'jcs-helm-find-files-navigate-forward)

(defun jcs-helm-find-files-navigate-back (orig-fun &rest args)
  "Advice run around `helm-ff-delete-char-backward' command."
  (if (jcs-current-char-equal-p "/")
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'jcs-helm-find-files-navigate-back)


(defvar jcs-helm-ff-other-window nil "Helm find file other window flag.")
(defvar jcs-helm-find-files-active nil "Helm find file flag.")

(defun jcs-helm-find-files-hook ()
  "Hook after `helm-find-files' initialized."
  ;; SEE: `jcs-key.el' file, and `minibuffer-setup-hook'.
  (setq jcs-helm-find-files-active t)
  )
(add-hook 'helm-find-files-after-init-hook 'jcs-helm-find-files-hook)

;;;###autoload
(defun jcs-helm-find-files-other-window ()
  "Find the file with Helm and open another window."
  (interactive)
  (setq jcs-helm-ff-other-window t)
  (helm-find-files nil))

;;;###autoload
(defun jcs-helm-exit-minibuffer ()
  "The command `helm-exit-minibuffer' calls interactively."
  (interactive)
  (helm-exit-minibuffer))

;;;###autoload
(defun jcs-helm-projectile-find-file-other-window ()
  "Find file in project to other window."
  (interactive)
  (let ((record-dd default-directory)
        (found-file nil)
        (starting-window (selected-window)))
    (jcs-other-window-next 1 t)
    (setq default-directory record-dd)
    (setq found-file (helm-projectile-find-file))
    (unless found-file
      (select-window starting-window))))

;;----------------------------------------------------------------------------
;; Adjust Scroll after selecting

(defun jcs--helm-adjust-scroll-window ()
  "Auto adjust the scrolling window to fit bottom."
  (when (helm--alive-p)
    (save-selected-window
      (jcs-jump-shown-to-buffer helm-buffer)
      (let* ((cur-line (line-number-at-pos))
             (last-line (line-number-at-pos (point-max)))
             (is-last-selection (= cur-line (1- last-line))))
        (if is-last-selection
            (jcs-recenter-top-bottom 'bottom)
          (let* ((first-vis-line (jcs-first-visible-line-in-window))
                 (last-vis-line (jcs-last-visible-line-in-window))
                 (win-lh (jcs--helm-find-window-line-height))
                 (lines-to-scroll (- win-lh (- last-vis-line first-vis-line))))
            (when (and (jcs-is-positive lines-to-scroll)
                       (> last-line win-lh)  ; Ensure no `auto-resize' occured.
                       (not (= cur-line first-vis-line)))
              (ignore-errors (scroll-down-line lines-to-scroll)))))))))

(defvar jcs--helm-adjust-scroll-window-timer nil
  "Auto adjust scroll window timer.")

(defvar jcs--helm-adjust-scroll-window-time 0.05
  "Auto adjust scroll window time.")

(defun jcs--helm-move-selection-after-hook ()
  "Move selection hook in `helm' buffer."
  ;;(jcs--helm-adjust-scroll-window)
  (when (timerp jcs--helm-adjust-scroll-window-timer)
    (cancel-timer jcs--helm-adjust-scroll-window-timer))
  (setq jcs--helm-adjust-scroll-window-timer
        (run-with-idle-timer jcs--helm-adjust-scroll-window-time
                             nil
                             'jcs--helm-adjust-scroll-window))
  )
(add-hook 'helm-move-selection-after-hook 'jcs--helm-move-selection-after-hook)


(provide 'jcs-helm-func)
;;; jcs-helm-func.el ends here
