;;; jcs-message-mode.el --- Message mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'message)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'message-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))

(provide 'jcs-message-mode)
;;; jcs-message-mode.el ends here
