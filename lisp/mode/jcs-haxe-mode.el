;;; jcs-haxe-mode.el --- Haxe mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'haxe-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-haxe-template "haxe" "default.txt"
  "Template for Haxe.")

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
  (jcs-key-local
    `(((kbd "<up>")        . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>")      . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "<backspace>") . jcs-smart-backspace)
      ((kbd "<delete>")    . jcs-smart-delete)

      ((kbd "DEL")         . jcs-electric-backspace)

      ((kbd "C-v")         . jcs-smart-yank))))

(provide 'jcs-haxe-mode)
;;; jcs-haxe-mode.el ends here
