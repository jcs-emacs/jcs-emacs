;;; jcs-ada-mode.el --- Ada mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ada-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-ada-template "ada" "default.txt"
  "Template for Ada.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ada-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]ads")
                              'jcs-insert-ada-template))

(provide 'jcs-ada-mode)
;;; jcs-ada-mode.el ends here
