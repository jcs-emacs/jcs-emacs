;;; jcs-groovy-mode.el --- Groovy mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'groovy-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-groovy-mode-hook ()
  "Hook for `groovy-mode'."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]groovy"
                                "[.]gradle")
                              'jcs-insert-groovy-template)

  ;; comment block
  (define-key groovy-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key groovy-mode-map (kbd "*") #'jcs-c-comment-pair))

(add-hook 'groovy-mode-hook 'jcs-groovy-mode-hook)

(provide 'jcs-groovy-mode)
;;; jcs-groovy-mode.el ends here
