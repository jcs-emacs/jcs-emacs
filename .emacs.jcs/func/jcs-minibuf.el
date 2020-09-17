;;; jcs-minibuf.el --- Minibuffer related settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jcs-minibuf--setup-hook ()
  "Hook when minibuffer setup."
  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (progn
    (jcs-gc-cons-threshold-speed-up t))

  (jcs-dark-blue-mode-line)

  (setq jcs-minibuf--setup-for-ivy-p t)

  ;; Register hook.
  (add-hook 'post-command-hook #'jcs-minibuffer--post-command nil t))
(add-hook 'minibuffer-setup-hook 'jcs-minibuf--setup-hook)

(defun jcs-minibuf--exit-hook ()
  "Hook when exit minibuffer."
  (jcs-reload-active-mode)

  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (progn
    (garbage-collect)
    (jcs-gc-cons-threshold-speed-up nil)))
(add-hook 'minibuffer-exit-hook 'jcs-minibuf--exit-hook)

(defun jcs-minibuffer--post-command ()
  "Minibuffer post command hook."
  (jcs-minibuf--ivy-post-command))

;;; Util

(defun jcs-minibuf-window-p (&optional win)
  "Check if WIN minibuffer window."
  (unless win (setq win (selected-window)))
  (eq win (minibuffer-window)))

;;; Ivy

(defvar jcs-minibuf--setup-for-ivy-p nil
  "Flag to check if execute setup for `ivy-mode'.")

(defun jcs-ivy--exhibit ()
  "Advice execute after `ivy--exhibit' function."
  (when jcs-minibuf--setup-for-ivy-p
    (setq jcs-minibuf--setup-for-ivy-p nil)
    (jcs-minibuf--ivy-setup)))

(advice-add 'ivy--exhibit :after #'jcs-ivy--exhibit)

(defun jcs-minibuf--ivy-setup ()
  "Setup hook for Ivy in minibuffer."
  (when ivy-mode
    (jcs-walk-through-all-windows-once
     (lambda ()
       (save-excursion
         (let* ((cur-ln (line-number-at-pos (point) t))
                (last-display-ln (jcs-last-visible-line-in-window))
                (first-display-ln (jcs-first-visible-line-in-window))
                (max-ln (line-number-at-pos (point-max) t))
                (visible-win-height (- max-ln first-display-ln)))
           (when (and (<= last-display-ln cur-ln)
                      (<= (window-body-height) visible-win-height))
             (jcs-recenter-top-bottom 'bottom))))))))

(defun jcs-minibuf--ivy-post-command ()
  "Post command for Ivy in minibuffer."
  (when ivy-mode
    (cond ((jcs-is-finding-file-p)
           (when (and (save-excursion (search-backward "~//" nil t))
                      (not (jcs-current-char-equal-p "/")))
             (save-excursion
               (forward-char -1)
               (backward-delete-char 1)))))))

(provide 'jcs-minibuf)
;;; jcs-minibuf.el ends here
