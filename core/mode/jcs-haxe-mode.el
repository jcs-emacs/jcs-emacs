;;; jcs-haxe-mode.el --- Haxe mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'haxe-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-haxe-template ()
  "Template for Haxe."
  (jcs--file-header--insert "haxe" "default.txt"))

;;
;; (@* "Hook" )
;;

(add-hook 'haxe-mode-hook 'jcs-prog-mode-hook)

(jcs-add-hook 'haxe-mode-hook
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]hx")
                              'jcs-insert-haxe-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "<backspace>") #'jcs-smart-backspace)
  (jcs-bind-key (kbd "<delete>") #'jcs-smart-delete)

  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  (jcs-bind-key (kbd "C-v") #'jcs-smart-yank)

  ;; switch frame.
  (jcs-bind-key (kbd "M-w") #'jcs-other-window-next)
  (jcs-bind-key (kbd "M-q") #'jcs-other-window-prev))

(provide 'jcs-haxe-mode)
;;; jcs-haxe-mode.el ends here
