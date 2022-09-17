;;; jcs-groovy-mode.el --- Groovy mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'groovy-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-groovy-template "groovy" "default.txt"
  "Header for Groovy header file.")

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
