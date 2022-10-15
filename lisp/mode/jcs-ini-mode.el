;;; jcs-ini-mode.el --- INI mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ini-mode)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ini-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))

(provide 'jcs-ini-mode)
;;; jcs-ini-mode.el ends here
