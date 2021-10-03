;;; build.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst jcs-ci t
  "Flag for CI testing.")

;; Workaround for Windows CI.
;; See https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
(setq network-security-level 'low)

;; Start regular Emacs file.
(load-file (expand-file-name "~/.emacs"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
