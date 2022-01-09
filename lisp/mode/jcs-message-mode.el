;;; jcs-message-mode.el --- Message mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'message)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'message-mode-hook
  (electric-pair-mode nil)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next)))))

(provide 'jcs-message-mode)
;;; jcs-message-mode.el ends here
