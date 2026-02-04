;;; email/message/config.el  -*- lexical-binding: t; -*-

(require 'message)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'message-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))

(msg-clean-add-echo-commands '( mail-send))
