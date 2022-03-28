;;; test-speed.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)

(load (concat user-emacs-directory "bin/test-startup.el"))

(defvar startup-time (format "%0.5f" (string-to-number (emacs-init-time)))
  "Total startup time.")

(message "[INFO] Start within %s" startup-time)

;;
;;; Generate speed badge

(defconst svg-url-format
  "https://img.shields.io/badge/%s-%s-green.svg"
  "Format to generate badges.")

(defconst output-dir "./docs/badges/speed/"
  "Where the badges store.")

(ignore-errors (delete-directory output-dir t))  ; clean up first
(make-directory output-dir t)                    ; recursive

(let ((os (format "%s" jcs-system-type))
      (file (concat output-dir os ".svg"))
      (url (format svg-url-format os startup-time)))
  (message "Downloading SVG from `%s` to `%s`... done!" url file)
  (url-copy-file url file))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; test-speed.el ends here
