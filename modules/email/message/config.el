;;; email/message/config.el  -*- lexical-binding: t; -*-

(require 'message)

;; Auto set to default.
(setq message-auto-save-directory
      (file-name-as-directory (expand-file-name "drafts" message-directory)))

(msg-clean-add-echo-commands '( mail-send))

;; Select account before writing email.
(gnus-select-account-enable)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'message-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line)
      ((kbd "C-a")    . mark-whole-buffer))))
