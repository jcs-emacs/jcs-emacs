;;; tools/csv/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'csv-mode-hook
  (csv-align-mode 1)
  (rainbow-csv-mode 1))
