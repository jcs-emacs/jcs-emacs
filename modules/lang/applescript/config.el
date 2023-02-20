;;; lang/applescript/config.el  -*- lexical-binding: t; -*-

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
