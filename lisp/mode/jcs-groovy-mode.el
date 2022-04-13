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

(jcs-add-hook 'groovy-mode-hook
  (setq-local docstr-show-type-name nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]groovy"
                                "[.]gradle")
                              'jcs-insert-groovy-template))

(provide 'jcs-groovy-mode)
;;; jcs-groovy-mode.el ends here
