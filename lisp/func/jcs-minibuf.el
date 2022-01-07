;;; jcs-minibuf.el --- Minibuffer related settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Echo Area" )
;;

(defconst jcs-echo-area-buffer-name " *Minibuf-0*"
  "Name of the minibuffer echo area buffer.")

(defun jcs-echo-area--init ()
  "Initialize echo area."
  (with-current-buffer jcs-echo-area-buffer-name
    (add-hook 'window-size-change-functions #'jcs-minibuf--window-size-change nil t)))

;;
;; (@* "Minibuffer" )
;;

(defconst jcs-minibuf-buffer-name " *Minibuf-1*"
  "Name of the minibuffer buffer.")

(defvar jcs-minibuf-enabled-p nil
  "Flag to see if minibuffer is enabled.")

(defun jcs-minibuf--init ()
  "Initialize minibuffer."
  (with-current-buffer jcs-minibuf-buffer-name
    (add-hook 'window-size-change-functions #'jcs-minibuf--window-size-change nil t)))

(jcs-advice-add 'ivy--minibuffer-setup :after
  ;; Ivy minibuffer setup hook.
  (setq jcs-minibuf-enabled-p t))

(jcs-add-hook 'minibuffer-setup-hook
  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (jcs-gc-cons-threshold-speed-up t)

  (jcs-dark-blue-mode-line)

  (jcs-echo-area--init)
  (jcs-minibuf--init)

  (setq jcs-minibuf--setup-for-ivy-p t)

  ;; Register hook.
  (add-hook 'post-command-hook #'jcs-minibuffer--post-command nil t))

(jcs-add-hook 'minibuffer-exit-hook
  (jcs-reload-active-mode)

  (setq jcs-minibuf-enabled-p nil)

  (jcs-dashboard-refresh-buffer)

  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (garbage-collect)
  (jcs-gc-cons-threshold-speed-up nil))

(defun jcs-minibuffer--post-command ()
  "Minibuffer post command hook."
  (jcs-minibuf--ivy-post-command))

;;
;; (@* "Util" )
;;

(defun jcs-minibuf-prompt-p ()
  "Return non-nil if current state is asking user for input."
  (string= (buffer-name) jcs-minibuf-buffer-name))

(defun jcs-minibuf-window-p (&optional win)
  "Check if WIN minibuffer window."
  (unless win (setq win (selected-window)))
  (eq win (minibuffer-window)))

;;
;; (@* "Window" )
;;

(defun jcs-minibuf--window-size-change (&rest _)
  "Hook for echo area when window size changed."
  (jcs-minibuf--window-setup))

(defun jcs-minibuf--window-setup ()
  "Resize window for minibuffer and echo area."
  (jcs-walk-windows
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

;;
;; (@* "Ivy" )
;;

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
