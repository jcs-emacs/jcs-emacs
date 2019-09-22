;;; build.el --- Test the configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defconst jcs-build-test t
  "Define for build testing.")

(setq package-check-signature nil)

;; Start regular Emacs file.
(load-file (expand-file-name "~/.emacs"))


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
