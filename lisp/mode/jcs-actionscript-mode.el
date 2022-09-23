;;; jcs-actionscript-mode.el --- ActionScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'actionscript-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-actionscript-template "actionscript" "default.txt"
  "Template for ActionScript.")

;;
;; (@* "Hook" )
;;

(add-hook 'actionscript-mode-hook 'jcs-prog-mode-hook)

(jcs-add-hook 'actionscript-mode-hook
  (jcs-use-cc-mutliline-comment)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]as")
                              'jcs-insert-actionscript-template)

  (jcs-key-local
    `(((kbd "<up>")        . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>")      . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "<backspace>") . jcs-smart-backspace)
      ((kbd "<delete>")    . jcs-smart-delete)
      ((kbd "SPC")         . jcs-smart-space)
      ((kbd "DEL")         . jcs-electric-backspace)
      ((kbd "C-v")         . jcs-smart-yank))))

(provide 'jcs-actionscript-mode)
;;; jcs-actionscript-mode.el ends here
