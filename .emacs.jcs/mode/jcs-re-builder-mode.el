;;; jcs-re-builder-mode.el --- RE-Builder mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 're-builder)

;;
;; (@* "Hook" )
;;

(defun jcs-re-builder-mode-hook ()
  "Mode hook for `RE-Builder-mode'."

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "M-k") #'jcs-reb-maybe-kill-this-buffer))

(add-hook 'reb-mode-hook 'jcs-re-builder-mode-hook)

(provide 'jcs-re-builder-mode)
;;; jcs-re-builder-mode.el ends here
