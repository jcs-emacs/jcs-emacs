;;; jcs-minibuf.el --- Minibuffer related settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jcs-minibuffer-window-p (&optional win)
  "Check if WIN minibuffer window."
  (unless win (setq win (selected-window)))
  (eq win (minibuffer-window)))

(defun jcs-minibuffer-setup-hook ()
  "Hook when minibuffer setup."
  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (progn
    (jcs-gc-cons-threshold-speed-up t))

  (jcs-dark-blue-mode-line)

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
             (jcs-recenter-top-bottom 'bottom)))))))

  ;; Register hook.
  (add-hook 'post-command-hook #'jcs-minibuffer--post-command-hook nil t))
(add-hook 'minibuffer-setup-hook 'jcs-minibuffer-setup-hook)

(defun jcs-minibuffer--post-command-hook ()
  "Minibuffer post command hook."
  (when ivy-mode
    (cond ((jcs-is-finding-file-p)
           (when (and (save-excursion (search-backward "~//" nil t))
                      (not (jcs-current-char-equal-p "/")))
             (save-excursion
               (forward-char -1)
               (backward-delete-char 1)))))))

(defun jcs-minibuffer-exit-hook ()
  "Hook when exit minibuffer."
  (jcs-reload-active-mode)

  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (progn
    (garbage-collect)
    (jcs-gc-cons-threshold-speed-up nil)))
(add-hook 'minibuffer-exit-hook 'jcs-minibuffer-exit-hook)

(provide 'jcs-minibuf)
;;; jcs-minibuf.el ends here
