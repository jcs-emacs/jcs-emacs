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
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))

(provide 'jcs-haxe-mode)
;;; jcs-haxe-mode.el ends here
