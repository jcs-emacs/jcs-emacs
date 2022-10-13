;;; jcs-dockerfile-mode.el --- Dokerfile mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dockerfile-mode)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'dockerfile-mode-hook
  (company-fuzzy-backend-add 'company-dockerfile))

(provide 'jcs-dockerfile-mode)
;;; jcs-dockerfile-mode.el ends here
