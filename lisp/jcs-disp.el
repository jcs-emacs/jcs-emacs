;;; jcs-disp.el --- Customize display format  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Mode State" )
;;

(defun jcs-reload-active-mode ()
  "Reload the active mode.
Note this is opposite logic to the toggle mode function."
  (interactive)
  (when (featurep 'jcs-modeline)
    (msgu-silent
      (cond
       ((jcs-funcall-fboundp #'jcs-backtrace-occurs-p) (jcs-hit-backtrace))
       ((active-minibuffer-window) (jcs-modeline-dark-blue))
       ((ignore-errors (jcs-funcall-fboundp #'dap--cur-active-session-or-die))
        (jcs-modeline-dark-orange))
       ((jcs-funcall-fboundp #'zoom-window--enable-p) (jcs-modeline-dark-green))
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
  (setq moody-mode-line-height 26
        x-underline-at-descent-line t)
  :config
  ;; XXX For issue, https://github.com/tarsius/moody/pull/41
  (jcs-advice-add 'moody-redisplay :around
    (let ((inhibit-redisplay t)) (apply arg0 args))))

(use-package jcs-modeline
  :init
  (setq jcs-modeline-show-point t
        jcs-modeline-show-mode-icons t))

;;
;; (@* "Echo Bar" )
;;

(use-package echo-bar
  :init
  (setq echo-bar-right-padding 0
        echo-bar-minibuffer nil))

(use-package region-state
  :hook (activate-mark . region-state-mode))

(provide 'jcs-disp)
;;; jcs-disp.el ends here
