;;; jcs-theme.el --- Theme definition  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Core" )
;;

(defun jcs-theme-current ()
  "Return current theme name."
  (or (nth 0 custom-enabled-themes) jcs-theme-default))

(defun jcs-light-theme-p ()
  "Return non-nil if current theme is light theme."
  (ignore-errors (elenv-light-color-p (face-background 'default))))

(defun jcs-theme-call (fnc)
  "Execute FNC with default arguments."
  (funcall fnc (jcs-theme-current)))

(defun jcs-theme-refresh ()
  "Refresh theme."
  (interactive)
  (load-theme (jcs-theme-current) t))

(defun jcs-setup-default-theme ()
  "Set default theme color."
  (interactive)
  (load-theme jcs-theme-default t))

(defun jcs-toggle-theme-light-dark ()
  "Toggle light/dark theme."
  (interactive)
  (if (jcs-light-theme-p) (load-theme 'vs-dark)
    (load-theme 'vs-light)))

;;
;; (@* "Load" )
;;

(defvar jcs-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun jcs-run-after-load-theme-hook ()
  "Load after load theme hook."
  (run-hook-with-args 'jcs-after-load-theme-hook (jcs-theme-current)))

(jcs-advice-add 'load-theme :after
  (jcs-set-font-size)
  (jcs-run-after-load-theme-hook)
  (jcs-reload-active-mode))

(provide 'jcs-theme)
;;; jcs-theme.el ends here
