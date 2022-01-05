;;; jcs-agda-mode.el --- Agda mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'agda-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-agda-template ()
  "Template for Agda."
  (jcs--file-header--insert "agda" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'agda2-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]agda" "[.]lagda")
                              'jcs-insert-agda-template))

(provide 'jcs-agda-mode)
;;; jcs-agda-mode.el ends here
