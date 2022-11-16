;;; emacs/backtrace/config.el  -*- lexical-binding: t; -*-

(defconst jcs-backtrace-buffer-name "*Backtrace*"
  "Name of the backtrace buffer.")

(defun jcs-hit-backtrace ()
  "Do stuff when backtrace occures."
  (jcs-modeline-red)  ; When error, turn red
  (msgu-inhibit-log
    (message "[INFO] Oops, error occurs! Please see backtrace for more information")))

(defun jcs-backtrace-occurs-p ()
  "Check if the backtrace occurs."
  (jcs-with-current-buffer jcs-backtrace-buffer-name
    (not (string-empty-p (buffer-string)))))

(defun jcs-backtrace-exit ()
  "Exit backtrace."
  (jcs-when-buffer-window jcs-backtrace-buffer-name
    (let (buffer-read-only) (erase-buffer) (bury-buffer))
    (unless (window-full-height-p) (delete-window))))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'backtrace-mode-hook
  (jcs-key-local
    `(((kbd "M-k") . kill-buffer-and-window))))
