;;; build.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst jcs-ci t
  "Flag for CI testing.")

;; Workaround for Windows CI
;; See https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
(setq network-security-level 'low)

(load-file (expand-file-name "~/.emacs"))  ; Start regular Emacs file

;; Log out init info
(add-hook 'after-init-hook
          (lambda ()
            (jcs-dashboard-init-info)
            (message "[INFO] %s" dashboard-init-info)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
