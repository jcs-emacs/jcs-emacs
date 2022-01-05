;;; jcs-applescript-mode.el --- AppleScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'applescript-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-applescript-template ()
  "Template for AppleScript."
  (jcs--file-header--insert "applescript" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'applescript-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]applescript" "[.]scpt" "[.]scptd")
                              'jcs-insert-applescript-template))

(provide 'jcs-applescript-mode)
;;; jcs-applescript-mode.el ends here
