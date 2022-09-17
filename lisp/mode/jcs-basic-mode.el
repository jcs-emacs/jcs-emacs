;;; jcs-basic-mode.el --- BASIC mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'basic-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-basic-template "basic" "default.txt"
  "Header format for BASIC file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'basic-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]bas")
                              'jcs-insert-basic-template))

(provide 'jcs-basic-mode)
;;; jcs-basic-mode.el ends here
