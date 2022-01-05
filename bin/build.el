;;; build.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'url-vars)

;; Workaround for Windows CI
;; See https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
(setq network-security-level 'low)

(defconst jcs-ci t
  "Flag for CI testing.")

(let (debug-on-error
      url-show-status
      (user-emacs-directory default-directory)
      (early-init-file (locate-user-emacs-file "early-init.el"))
      (user-init-file (locate-user-emacs-file "init.el")))
  (message "Early Exist: %s" (file-exists-p early-init-file))
  (message "Exist: %s" (file-exists-p user-init-file))
  (load early-init-file)
  (load user-init-file)  ; Start regular Emacs file
  (run-hooks after-init-hook)
  (run-hooks emacs-startup-hook))

(jcs-emacs-version)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
