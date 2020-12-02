;;; jcs-r-mode.el --- R programming language mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ess-r-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-r-mode-hook ()
  "R mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]r")
                              'jcs-insert-r-template))

(add-hook 'ess-r-mode-hook 'jcs-r-mode-hook)

(provide 'jcs-r-mode)
;;; jcs-r-mode.el ends here
