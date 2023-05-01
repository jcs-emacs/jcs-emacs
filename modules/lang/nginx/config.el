;;; lang/nginx/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-nginx-template "nginx" "default.txt"
  "Header for Nginx header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nginx-mode-hook
  (company-fuzzy-backend-add-before 'company-nginx 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]conf")
                              'jcs-insert-nginx-template))
