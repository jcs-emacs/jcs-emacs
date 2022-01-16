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

(jcs-add-hook 'minibuffer-setup-hook
  (jcs-gc-cons-threshold-speed-up t)  ; Avoid GCs while using `vertico'
  (setq jcs-minibuf-enabled-p t)
  (jcs-dark-blue-mode-line)
  (jcs-echo-area--init)
  (jcs-minibuf--init)
  (add-hook 'post-command-hook #'jcs-minibuffer--post-command nil t))

(jcs-add-hook 'minibuffer-exit-hook
  (jcs-reload-active-mode)
  (setq jcs-minibuf-enabled-p nil)
  (jcs-dashboard-refresh-buffer)
  (garbage-collect)  ; Restore GC
  (jcs-gc-cons-threshold-speed-up nil))

(defvar jcs-minibuffer-post-command-hook nil
  "Post command hook inside minibuffer.")

(defun jcs-minibuffer--post-command ()
  "Minibuffer post command hook."
  (run-hooks 'jcs-minibuffer-post-command-hook))

;;
;; (@* "Util" )
;;

(defun jcs-minibuf-prompt-p ()
  "Return non-nil if current state is asking user for input."
  (string= (buffer-name) jcs-minibuf-buffer-name))

;;
;; (@* "Window" )
;;

(defun jcs-minibuf--window-size-change (&rest _)
  "Hook for echo area when window size changed."
  )

(provide 'jcs-minibuf)
;;; jcs-minibuf.el ends here
