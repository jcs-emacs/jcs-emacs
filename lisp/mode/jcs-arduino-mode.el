;;; jcs-arduino-mode.el --- Arduino mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'arduino-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-arduino-template ()
  "Template for Arduino."
  (jcs--file-header--insert "arduino" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'arduino-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]pde" "[.]ino")
                              'jcs-insert-arduino-template))

(provide 'jcs-arduino-mode)
;;; jcs-arduino-mode.el ends here
