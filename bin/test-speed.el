;;; test-speed.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)

(load (concat user-emacs-directory "bin/test-startup.el"))

(message "[INFO] Start within %s" (format "%0.5f" (string-to-number (emacs-init-time))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; test-speed.el ends here
