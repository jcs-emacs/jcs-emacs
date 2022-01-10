;;; jcs-ini-mode.el --- INI mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ini-mode)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ini-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next)))))

(provide 'jcs-ini-mode)
;;; jcs-ini-mode.el ends here
