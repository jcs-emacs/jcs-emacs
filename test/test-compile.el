;;; test-compile.el --- Byte compile concatenate file  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Test the concatened file built from `eask concat`.
;;
;; We are only going to check for rule `redefine', and shall be sufficient for
;; regular Emacs configuration.
;;

;;; Code:

(setq byte-compile-error-on-warn t)

(let* ((concated-file "./dist/jcs-emacs.built.el")
       (concated-file (expand-file-name concated-file))
       (conditions "
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (redefine)
;; End:
"))
  (with-current-buffer (find-file concated-file)
    (unless (string-suffix-p conditions (buffer-string))
      (goto-char (point-max))
      (insert conditions))
    (save-buffer)
    (byte-compile-file buffer-file-name)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; test-compile.el ends here
