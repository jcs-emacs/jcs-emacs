;;; email/message/config.el  -*- lexical-binding: t; -*-

(require 'message)

;;
;; (@* "Hook" )
;;

(setq message-auto-save-directory nil)

(jcs-add-hook 'message-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line)
      ((kbd "C-a")    . mark-whole-buffer))))

(msg-clean-add-echo-commands '( mail-send))
