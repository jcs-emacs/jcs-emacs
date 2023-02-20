;;; lang/autoconf/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'autoconf-mode-hook
  (company-fuzzy-backend-add 'company-autoconf))
