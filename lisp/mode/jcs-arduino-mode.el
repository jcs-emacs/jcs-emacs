;;; jcs-arduino-mode.el --- Arduino mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'arduino-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-arduino-template "arduino" "default.txt"
  "Template for Arduino.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'arduino-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]pde" "[.]ino")
                              'jcs-insert-arduino-template))

(provide 'jcs-arduino-mode)
;;; jcs-arduino-mode.el ends here
