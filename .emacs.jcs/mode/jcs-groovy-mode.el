;;; jcs-groovy-mode.el --- Groovy mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'groovy-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-groovy-template ()
  "Header for Groovy header file."
  (jcs--file-header--insert "groovy" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-groovy-mode-hook ()
  "Hook for `groovy-mode'."

  (setq-local docstr-show-type-name nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]groovy"
                                "[.]gradle")
                              'jcs-insert-groovy-template))

(add-hook 'groovy-mode-hook 'jcs-groovy-mode-hook)

(provide 'jcs-groovy-mode)
;;; jcs-groovy-mode.el ends here
