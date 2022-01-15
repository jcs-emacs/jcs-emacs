;;; test-startup.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(message "[TEST] %s" (fboundp 'font-info))

(font-info (face-font 'mode-line))


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; test-startup.el ends here
