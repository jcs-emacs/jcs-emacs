;;; jcs-scala-mode.el --- Scala mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'scala-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-scala-template ()
  "Header for Scala header file."
  (jcs--file-header--insert "scala" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'scala-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]scala")
                              'jcs-insert-scala-template))

(provide 'jcs-scala-mode)
;;; jcs-scala-mode.el ends here
