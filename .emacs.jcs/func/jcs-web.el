;;; jcs-web.el --- Web Development related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Impatient Mode" )
;;

(defun jcs-impatient-mode (args)
  "Default `impatient-mode' function by ARGS."
  (if (= args 1)
      (progn
        (unless (process-status "httpd") (httpd-start))
        (impatient-mode args)
        (imp-set-user-filter nil)
        (imp-visit-buffer))
    (httpd-stop)
    (impatient-mode args)))

(defun jcs-impatient-by-mode (args)
  "Enable/Disable `impatient-mode' by ARGS"
  (require 'impatient-mode)
  (cond ((jcs-is-current-major-mode-p '("markdown-mode"))
         (impatient-showdown-mode args))
        (t (jcs-impatient-mode args))))

(defun jcs-impatient-start ()
  "Start real time editing with default port."
  (interactive)
  (jcs-impatient-by-mode 1)
  (message "[INFO] Start real time editing with port: %d" httpd-port httpd-port))

(defun jcs-impatient-stop ()
  "Shutdown real time editing with default port."
  (interactive)
  (jcs-impatient-by-mode -1)
  (message "[INFO] Shutdown real time editing with port: %d" httpd-port))

;;
;; (@* "Other" )
;;

(defun jcs-toggle-web-mode-offsetless-elements ()
  "Toggle between indent with html tag or not to."
  (interactive)
  (if (get 'jcs-toggle-web-mode-offsetless-elements 'state)
      (progn
        (dolist (tmp-element jcs-web-mode-offsetless-elements-toggle)
          (push tmp-element web-mode-offsetless-elements))
        (put 'jcs-toggle-web-mode-offsetless-elements 'state nil))
    (dolist (tmp-element jcs-web-mode-offsetless-elements-toggle)
      (setq web-mode-offsetless-elements (remove tmp-element web-mode-offsetless-elements)))
    (put 'jcs-toggle-web-mode-offsetless-elements 'state t)))

(defun jcs-emmet-expand-line ()
  "Wrapper of `emmet-expand-line' function."
  (interactive)
  (if (jcs-current-point-face 'link)
      (call-interactively #'goto-address-at-point)
    (unless (call-interactively #'emmet-expand-line)
      (jcs-ctrl-return-key))))

(provide 'jcs-web)
;;; jcs-web.el ends here
