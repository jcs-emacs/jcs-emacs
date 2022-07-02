;;; jcs-dockerfile-mode.el --- Dokerfile mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dockerfile-mode)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'dockerfile-mode-hook
  (company-fuzzy-backend-add 'company-dockerfile)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next)))))

(provide 'jcs-dockerfile-mode)
;;; jcs-dockerfile-mode.el ends here
