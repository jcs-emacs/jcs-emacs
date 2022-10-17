;;; jcs-terraform-mode.el --- YAML mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'terraform-mode)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'terraform-mode-hook
  (company-fuzzy-backend-add 'company-terraform))

(provide 'jcs-terraform-mode)
;;; jcs-terraform-mode.el ends here
