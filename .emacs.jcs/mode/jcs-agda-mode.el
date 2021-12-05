;;; jcs-agda-mode.el --- Agda mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'agda2-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-agda-template ()
  "Template for Agda."
  (jcs--file-header--insert "agda" "default.txt"))

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
