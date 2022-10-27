;;; lang/terraform/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'terraform-mode-hook
  (company-fuzzy-backend-add 'company-terraform))
