;;; lang/dockerfile/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'dockerfile-mode-hook
  (company-fuzzy-backend-add 'company-dockerfile))
