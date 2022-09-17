;;; jcs-d-mode.el --- D mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'd-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-d-template "d" "default.txt"
  "Template for D.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'd-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]d")
                              'jcs-insert-d-template))

(provide 'jcs-d-mode)
;;; jcs-d-mode.el ends here
