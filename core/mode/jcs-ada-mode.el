;;; jcs-ada-mode.el --- Ada mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ada-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-ada-template ()
  "Template for Ada."
  (jcs--file-header--insert "ada" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ada-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]ads")
                              'jcs-insert-ada-template))

(provide 'jcs-ada-mode)
;;; jcs-ada-mode.el ends here
