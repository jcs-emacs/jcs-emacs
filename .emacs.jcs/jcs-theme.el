;;; jcs-theme.el --- Theme definition  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Core" )
;;

(defun jcs-reset-plugins-base-on-theme ()
  "Reset certain plugins base on the theme."
  (jcs-reset-ahs-by-theme)
  (jcs-funcall-fboundp #'jcs-company-default-theme)
  (jcs-reset-dashboard-banner-by-theme)
  (jcs-reset-show-paren-by-theme)
  (jcs-reset-tabbar-theme)
  (jcs-reset-yascroll-color-by-theme)
  (jcs-reset-common-faces-by-theme)
  (jcs-reload-active-mode)
  (jcs-re-enable-mode-if-was-enabled 'highlight-indent-guides-mode))

(defun jcs-set-theme (cc hlc rc frc)
  "Setup the theme to all frames.

Arguments stand for these explanation,

  * CC - cursor color.
  * HLC - highlight line color.
  * RC - region color.
  * FRC - fringe color."
  (jcs-walk-frames
   (lambda ()
     (set-cursor-color cc)
     (with-eval-after-load 'hl-line (set-face-background 'hl-line hlc))
     (set-face-background 'region rc)
     (set-face-background 'fringe frc)
     (jcs-reset-plugins-base-on-theme))))

(defun jcs-vs-light-theme ()
  "Visual Studio IDE light theme."
  (interactive)
  (load-theme 'vs-light t)
  (jcs-set-theme "#909090" "#E6E6E6" "#99C9EF" "#E6E7E8"))

(defun jcs-vs-dark-theme ()
  "Visual Studio IDE dark theme."
  (interactive)
  (load-theme 'vs-dark t)
  (jcs-set-theme "#909090" "#2E2E2E" "#264F78" "#333333"))

(defun jcs-light-theme-p ()
  "Return non-nil if current theme is light theme."
  (ignore-errors (jcs-is-light-color-p (face-background 'default))))

(defun jcs-dark-theme-p ()
  "Return non-nil if current theme is dark theme."
  (not (jcs-light-theme-p)))

(defun jcs-refresh-theme ()
  "Refresh theme."
  (interactive)
  (if (jcs-light-theme-p) (jcs-vs-light-theme) (jcs-vs-dark-theme)))

(defun jcs-setup-default-theme ()
  "Set default theme color."
  (interactive)
  (jcs-vs-dark-theme))

;;
;; (@* "Load" )
;;

(defun jcs--enable-theme--advice-after (&rest _)
  "Advice execute after `load-theme' function."
  (jcs-set-font-size jcs-default-font-size)
  (jcs-walk-frames (lambda () (jcs-reset-plugins-base-on-theme))))
(advice-add 'enable-theme :after #'jcs--enable-theme--advice-after)

(provide 'jcs-theme)
;;; jcs-theme.el ends here
