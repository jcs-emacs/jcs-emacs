;;; lang/actionscript/config.el  -*- lexical-binding: t; -*-

(require 'cc-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-actionscript-template "actionscript" "default.txt"
  "Template for ActionScript.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'actionscript-mode-hook
  (run-hooks 'prog-mode-hook)

  (jcs-use-cc-mutliline-comment)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]as")
                              'jcs-insert-actionscript-template)

  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))
