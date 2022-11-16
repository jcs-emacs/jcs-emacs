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

(leaf minions
  :init
  (setq minions-mode-line-delimiters nil
        minions-mode-line-lighter ""))

(leaf moody
  :init
  (setq moody-mode-line-height 26
        x-underline-at-descent-line t)
  :defer-config
  ;; XXX For issue, https://github.com/tarsius/moody/pull/41
  (jcs-advice-add 'moody-redisplay :around
    (let ((inhibit-redisplay t)) (apply arg0 args)))
  (unless elenv-graphic-p (jcs-advice-add 'moody-tab :override arg0)))

(leaf jcs-modeline
  :defer-config
  (setq-default
   frame-title-format
   '((:eval invocation-name)
     " - "
     (:eval user-real-login-name) "@" (:eval system-name) ": "
     (:eval (when (buffer-modified-p) " *"))
     (:eval (if buffer-file-name "%f" "%b")))))

;;
;; (@* "Echo Bar" )
;;

(defun jcs-buffer-spaces-or-tabs ()
  "Check if buffer using spaces or tabs."
  (if (= (how-many "^\t" (point-min) (point-max)) 0) "SPC" "TAB"))

(leaf echo-bar
  :init
  (setq echo-bar-right-padding 0
        echo-bar-function
        (lambda ()  ; String to display in echo bar
          (format "%s: %s  %s  %s  %s"
                  (jcs-buffer-spaces-or-tabs)
                  (indent-control-get-indent-level-by-mode)
                  buffer-file-coding-system
                  (show-eol-get-eol-mark-by-system)
                  (format-time-string "%b %d, %Y %H:%M:%S")))
        echo-bar-minibuffer nil))

(leaf region-state
  :hook (activate-mark-hook . region-state-mode))

(provide 'jcs-disp)
;;; jcs-disp.el ends here
