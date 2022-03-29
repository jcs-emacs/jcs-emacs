;;; test-speed.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)

(load (concat user-emacs-directory "test/startup/test-startup.el"))

(defvar startup-time (format "%0.5f" (string-to-number (emacs-init-time)))
  "Total startup time.")

(message "[INFO] Start within %s" startup-time)

;;
;;; Generate speed badge

(defconst svg-url-format
  "https://img.shields.io/badge/%s-%s-%s.svg?logo=speedtest"
  "Format to generate badges.")

(defconst output-dir "./docs/badges/speed/"
  "Where the badges store.")

(ignore-errors (make-directory output-dir t))

(let* ((os (format "%s" jcs-system-type))
       (file (concat output-dir os ".svg"))
       (time (string-to-number startup-time))
       (color (cond ((< time 5.0)  "2DB94D")
                    ((< time 10.0) "CCD808")
                    (t             "C50900")))
       (url (format svg-url-format os startup-time color)))
  (message "Downloading SVG from `%s` to `%s`... done!" url file)
  (url-copy-file url file t))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; test-speed.el ends here
