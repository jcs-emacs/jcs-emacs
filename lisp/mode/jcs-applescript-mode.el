;;; jcs-applescript-mode.el --- AppleScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'applescript-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-applescript-template "applescript" "default.txt"
  "Template for AppleScript.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'applescript-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]applescript" "[.]scpt" "[.]scptd")
                              'jcs-insert-applescript-template))

(provide 'jcs-applescript-mode)
;;; jcs-applescript-mode.el ends here
