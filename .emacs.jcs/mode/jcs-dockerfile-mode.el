;;; jcs-dockerfile-mode.el --- Dokerfile mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'dockerfile-mode)

(require 'jcs-python-func)


(defun jcs-dockerfile-mode-hook ()
  "Dokerfile mode hook."

  ;; Normal
  (define-key dockerfile-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key dockerfile-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  )
(add-hook 'dockerfile-mode-hook 'jcs-dockerfile-mode-hook)


(provide 'jcs-dockerfile-mode)
;;; jcs-dockerfile-mode.el ends here
