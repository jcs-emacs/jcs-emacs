;;; jcs-basic-mode.el --- BASIC mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'basic-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-basic-template ()
  "Header format for BASIC file."
  (jcs--file-header--insert "basic" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'basic-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]bas")
                              'jcs-insert-basic-template))

(provide 'jcs-basic-mode)
;;; jcs-basic-mode.el ends here
