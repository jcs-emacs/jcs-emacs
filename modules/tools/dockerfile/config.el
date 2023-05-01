;;; lang/dockerfile/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'dockerfile-mode-hook
  (company-fuzzy-backend-add-before 'company-dockerfile 'company-dabbrev))
