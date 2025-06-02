;;; lang/magik/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-magik-template "magik" "default.txt"
  "Header for Magik header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'magik-mode-hook
  (company-fuzzy-backend-add-before 'company-magik 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]magik")
                              'jcs-insert-magik-template))
