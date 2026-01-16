;;; jcs-disp.el --- Customize display format  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Mode State" )
;;

(defun jcs-debugging-p ()
  "Return non-nil if current in debugging session."
  (or (elenv-debugging-p)
      (ignore-errors (jcs-fboundp-apply #'dap--cur-active-session-or-die))))

(defun jcs-reload-active-mode ()
  "Reload the active mode.
Note this is opposite logic to the toggle mode function."
  (interactive)
  (when (featurep 'jcs-modeline)
    (msgu-silent
      (cond
       ((jcs-fboundp-apply #'jcs-backtrace-occurs-p) (jcs-hit-backtrace))
       ((active-minibuffer-window) (jcs-modeline-dark-blue))
       ((jcs-debugging-p) (jcs-modeline-dark-orange))
       ((jcs-fboundp-apply #'zoom-window--enable-p) (jcs-modeline-dark-green))
       (t (jcs-modeline-gray))))))

;;
;; (@* "Modeline" )
;;

(use-package minions
  :init
  (setq minions-mode-line-delimiters nil
        minions-mode-line-lighter ""))

(use-package moody
  :init
  (setq moody-mode-line-height #'window-mode-line-height
        moody-slant-function #'jcs-modeline-moody-arrow
        x-underline-at-descent-line t)
  :config
  ;; XXX For issue, https://github.com/tarsius/moody/pull/41
  (jcs-advice-add 'moody-redisplay :around
    (let ((inhibit-redisplay t)) (apply arg0 args))))

(use-package jcs-modeline
  :init
  (setq jcs-modeline-show-point t
        jcs-modeline-show-mode-icons t
        jcs-modeline-show-mode-name t
        jcs-modeline-icon-scale-factor 0.85
        jcs-modeline-icon-v-adjust 0.1))

;;
;; (@* "Echo Bar" )
;;

(use-package echo-bar
  :init
  (setq echo-bar-right-padding (if elenv-graphic-p 0 1)
        echo-bar-minibuffer nil)
  :config
  (jcs-advice-add 'echo-bar-update :after
    (when auto-scroll-bar-mode
      (auto-scroll-bar--hide-minibuffer))))

(use-package region-state
  :hook (activate-mark . region-state-mode))

(provide 'jcs-disp)
;;; jcs-disp.el ends here
