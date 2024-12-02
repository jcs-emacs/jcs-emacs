;;; lang/jai/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-janet-template "janet" "default.txt"
  "Header for Janet header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'janet-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]janet")
                              'jcs-insert-janet-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-janet
  :hook (flycheck-mode . (lambda (&rest _) (require 'flycheck-janet))))
