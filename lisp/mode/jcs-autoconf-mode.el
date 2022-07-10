;;; jcs-autoconf-mode.el --- BASIC mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'autoconf-mode)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'autoconf-mode-hook
  (company-fuzzy-backend-add 'company-autoconf))

(provide 'jcs-autoconf-mode)
;;; jcs-autoconf-mode.el ends here
