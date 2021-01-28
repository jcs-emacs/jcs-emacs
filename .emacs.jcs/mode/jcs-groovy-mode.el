;;; jcs-groovy-mode.el --- Groovy mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'groovy-mode)

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
