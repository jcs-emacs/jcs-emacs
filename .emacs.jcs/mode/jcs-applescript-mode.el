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

(defun jcs-applescript-mode-hook ()
  "Hook for AppleScript."
  ;; File Header
  (jcs-insert-header-if-valid '("[.]applescript" "[.]scpt" "[.]scptd")
                              'jcs-insert-applescript-template))

(add-hook 'applescript-mode-hook 'jcs-applescript-mode-hook)

(provide 'jcs-applescript-mode)
;;; jcs-applescript-mode.el ends here
