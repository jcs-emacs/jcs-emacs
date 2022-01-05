;;; jcs-nginx-mode.el --- Nginx config  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'nginx-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-nginx-template ()
  "Header for Nginx header file."
  (jcs--file-header--insert "nginx" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nginx-mode-hook
  (jcs-company-safe-add-backend 'company-nginx)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]conf")
                              'jcs-insert-nginx-template))

(provide 'jcs-nginx-mode)
;;; jcs-nginx-mode.el ends here
