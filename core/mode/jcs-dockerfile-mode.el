;;; jcs-dockerfile-mode.el --- Dokerfile mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dockerfile-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-dockerfile-mode-hook ()
  "Dokerfile mode hook."

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(add-hook 'dockerfile-mode-hook 'jcs-dockerfile-mode-hook)

(provide 'jcs-dockerfile-mode)
;;; jcs-dockerfile-mode.el ends here
