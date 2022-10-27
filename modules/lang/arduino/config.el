;;; lang/arduino/config.el  -*- lexical-binding: t; -*-

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
