;;; lang/beancount/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-beancount-template "beancount" "default.txt"
  "Header format for Beancount file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'beancount-mode-hook
  (setq beancount-electric-currency t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]beancount")
                              'jcs-insert-beancount-template))
