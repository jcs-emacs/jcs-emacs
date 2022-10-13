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
    `(((kbd "<up>")   . jcs-smart-previous-line)
      ((kbd "<down>") . jcs-smart-next-line))))

(provide 'jcs-ini-mode)
;;; jcs-ini-mode.el ends here
