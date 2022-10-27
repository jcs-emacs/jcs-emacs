;;; lang/ini/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ini-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))
