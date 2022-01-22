;;; jcs-minibuf.el --- Minibuffer related settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Minibuffer" )
;;

(defvar jcs-minibuf--old-completion-style nil
  "Different completion style when completing using minbuffer.")

(jcs-add-hook 'minibuffer-setup-hook
  (jcs-gc-cons-threshold-speed-up t)  ; Avoid GCs while using `vertico'
  (setq jcs-minibuf--old-completion-style completion-styles
        completion-styles '(flx))
  (jcs-dark-blue-mode-line)
  (add-hook 'post-command-hook #'jcs-minibuffer--post-command nil t))

(jcs-add-hook 'minibuffer-exit-hook
  (jcs-reload-active-mode)
  (setq completion-styles jcs-minibuf--old-completion-style)
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

(defconst jcs-minibuf-buffer-name " *Minibuf-1*"
  "Name of the minibuffer buffer.")

(defun jcs-minibuf-prompt-p ()
  "Return non-nil if current state is asking user for input."
  (string= (buffer-name) jcs-minibuf-buffer-name))

(defun jcs-M-x-p ()
  "Return non-nil if current minibuffer M-x."
  (jcs-minibuf--compare-p "M-x" 'regex))

(defun jcs-finding-file-p ()
  "Return non-nil if current minibuffer finding file."
  (jcs-minibuf--compare-p "Find file" 'regex))

(defun jcs-renaming-p ()
  "Return non-nil if current minibuffer renaming."
  (jcs-minibuf--compare-p "New name:" 'regex))

(provide 'jcs-minibuf)
;;; jcs-minibuf.el ends here
