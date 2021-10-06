;;; jcs-agda-mode.el --- Agda mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'agda2-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-agda-mode-hook ()
  "Agda mode hook."
  ;; File Header
  (jcs-insert-header-if-valid '("[.]agda" "[.]lagda")
                              'jcs-insert-agda-template))

(add-hook 'agda2-mode-hook 'jcs-agda-mode-hook)

(provide 'jcs-agda-mode)
;;; jcs-agda-mode.el ends here
