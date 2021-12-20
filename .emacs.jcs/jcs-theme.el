;;; jcs-theme.el --- Theme definition  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Core" )
;;

(defun jcs-theme-call (fnc)
  "Execute FNC with default arguments."
  (funcall fnc (jcs-light-theme-p)))

(defun jcs-theme-reapply ()
  "Reset certain plugins base on the theme."
  (jcs-reset-common-faces-by-theme)
  (jcs-reload-active-mode)
  (jcs-re-enable-mode-if-was-enabled 'highlight-indent-guides-mode))

(defun jcs--set-theme (cc hlc rc frc)
  "Config the theme to all frames.

Arguments stand for these explanation,

  * CC - cursor color.
  * HLC - highlight line color.
  * RC - region color.
  * FRC - fringe color."
  (set-cursor-color cc)
  (with-eval-after-load 'hl-line (set-face-background 'hl-line hlc))
  (set-face-background 'region rc)
  (set-face-background 'fringe frc)
  (jcs-theme-reapply))

(add-hook 'jcs-after-load-theme-hook
          (lambda (light-p)
            (if light-p
                (jcs--set-theme "#909090" "#E6E6E6" "#99C9EF" "#E6E7E8")
              (jcs--set-theme "#909090" "#2E2E2E" "#264F78" "#333333"))))

(defun jcs-light-theme-p ()
  "Return non-nil if current theme is light theme."
  (ignore-errors (jcs-light-color-p (face-background 'default))))

(defun jcs-dark-theme-p ()
  "Return non-nil if current theme is dark theme."
  (not (jcs-light-theme-p)))

(defun jcs-refresh-theme ()
  "Refresh theme."
  (interactive)
  (load-theme (or (nth 0 custom-enabled-themes) jcs-theme-default) t))

(defun jcs-setup-default-theme ()
  "Set default theme color."
  (interactive)
  (load-theme jcs-theme-default t))

;;
;; (@* "Load" )
;;

(defvar jcs-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun jcs--enable-theme--advice-after (&rest _)
  "Advice execute after `load-theme' function."
  (jcs-set-font-size jcs-default-font-size)
  (let ((light-p (jcs-light-theme-p)))
    (jcs-walk-frames
     (lambda () (run-hook-with-args 'jcs-after-load-theme-hook light-p)))))
(advice-add 'enable-theme :after #'jcs--enable-theme--advice-after)

(provide 'jcs-theme)
;;; jcs-theme.el ends here
