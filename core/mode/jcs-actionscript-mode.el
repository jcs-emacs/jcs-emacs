;;; jcs-actionscript-mode.el --- ActionScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'actionscript-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-actionscript-template ()
  "Template for ActionScript."
  (jcs--file-header--insert "actionscript" "default.txt"))

;;
;; (@* "Hook" )
;;

(add-hook 'actionscript-mode-hook 'jcs-prog-mode-hook)

(jcs-add-hook 'actionscript-mode-hook
  (jcs-use-cc-mutliline-comment)

  (setq-local docstr-show-type-name nil)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]as")
                              'jcs-insert-actionscript-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "<backspace>") #'jcs-smart-backspace)
  (jcs-bind-key (kbd "<delete>") #'jcs-smart-delete)
  (jcs-bind-key (kbd "SPC") #'jcs-smart-space)

  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  (jcs-bind-key (kbd "C-v") #'jcs-smart-yank))

(provide 'jcs-actionscript-mode)
;;; jcs-actionscript-mode.el ends here
