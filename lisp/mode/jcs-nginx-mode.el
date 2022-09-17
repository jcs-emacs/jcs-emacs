;;; jcs-nginx-mode.el --- Nginx config  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'nginx-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-nginx-template "nginx" "default.txt"
  "Header for Nginx header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nginx-mode-hook
  (company-fuzzy-backend-add 'company-nginx)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]conf")
                              'jcs-insert-nginx-template))

(provide 'jcs-nginx-mode)
;;; jcs-nginx-mode.el ends here
