;;; tools/csv/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook '(csv-mode-hook tsv-mode-hook)
  (add-hook 'tree-sitter-hl-mode-hook #'rainbow-csv-mode nil t))
