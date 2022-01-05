;;; jcs-kotlin-mode.el --- Kotlin mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'kotlin-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-kotlin-template ()
  "Header for Kotlin header file."
  (jcs--file-header--insert "kotlin" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'kotlin-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]kt"
                                "[.]ktm"
                                "[.]kts")
                              'jcs-insert-kotlin-template))

(provide 'jcs-kotlin-mode)
;;; jcs-kotlin-mode.el ends here
