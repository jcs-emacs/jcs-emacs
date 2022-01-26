;;; jcs-re-builder-mode.el --- RE-Builder mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 're-builder)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'reb-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "M-k")    . kill-buffer-and-window))))

(provide 'jcs-re-builder-mode)
;;; jcs-re-builder-mode.el ends here
