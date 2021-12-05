;;; jcs-cobol-mode.el --- COBOL mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cobol-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-cobol-template ()
  "Template for COBOL."
  (jcs--file-header--insert "cobol" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-cobol-mode-hook ()
  "COBOL mode hook."
  (electric-pair-mode nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cbl")
                              'jcs-insert-cobol-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(add-hook 'cobol-mode-hook 'jcs-cobol-mode-hook)

(provide 'jcs-cobol-mode)
;;; jcs-cobol-mode.el ends here
