;;; lang/haxe/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-haxe-template "haxe" "default.txt"
  "Template for Haxe.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'haxe-mode-hook
  (run-hooks 'prog-mode-hook)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]hx")
                              'jcs-insert-haxe-template)

  ;; Normal
  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))
