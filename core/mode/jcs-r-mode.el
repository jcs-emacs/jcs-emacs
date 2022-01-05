;;; jcs-r-mode.el --- R programming language mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ess-r-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-r-template ()
  "Header for R header file."
  (jcs--file-header--insert "r" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ess-r-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]r")
                              'jcs-insert-r-template))

(provide 'jcs-r-mode)
;;; jcs-r-mode.el ends here
