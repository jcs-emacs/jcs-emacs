;;; jcs-message-mode.el --- Message mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'message)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'message-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . jcs-smart-previous-line)
      ((kbd "<down>") . jcs-smart-next-line))))

(provide 'jcs-message-mode)
;;; jcs-message-mode.el ends here
