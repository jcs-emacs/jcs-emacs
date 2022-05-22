;;; jcs-d-mode.el --- D mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'd-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-d-template ()
  "Template for D."
  (jcs--file-header--insert "d" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'd-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]d")
                              'jcs-insert-d-template))

(provide 'jcs-d-mode)
;;; jcs-d-mode.el ends here
