;;; test-packages.el --- Test the packages  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Test packages and generate packages count badge.
;;

;;; Code:

(load (concat user-emacs-directory "test/startup/test-startup.el"))

;;
;;; Generate packages count badge

(defconst svg-url-format
  "https://img.shields.io/badge/Packages-%s-6B8E23.svg?logo=hack-the-box"
  "Format to generate badges.")

(defconst output-dir "./docs/badges/"
  "Where the badges store.")

(ignore-errors (make-directory output-dir t))

(let* ((file (concat output-dir "packages.svg"))
       (url (format svg-url-format (length package-alist))))
  (message "Downloading SVG from `%s` to `%s`... done!" url file)
  (url-copy-file url file t))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; test-packages.el ends here
