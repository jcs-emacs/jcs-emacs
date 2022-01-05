;;; jcs-re-builder-mode.el --- RE-Builder mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 're-builder)

;;
;; (@* "Functions" )
;;

(defun jcs-reb-maybe-kill-this-buffer ()
  "Kill this buffer in `re-builder' mode."
  (interactive)
  (let (is-killed)
    (setq is-killed (jcs-maybe-kill-this-buffer))
    (when is-killed (delete-window))))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'reb-mode-hook
  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "M-k") #'jcs-reb-maybe-kill-this-buffer))

(provide 'jcs-re-builder-mode)
;;; jcs-re-builder-mode.el ends here
