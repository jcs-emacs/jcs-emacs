;;; lang/qss/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-qss-template "qss" "default.txt"
  "Template for QSS.")

;;
;; (@* "Hook" )
;;

(add-hook 'qss-mode-hook 'emmet-mode)

(jcs-add-hook 'qss-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]qss")
                              'jcs-insert-qss-template)

  (jcs-key-local
    `(((kbd "C-k s") . com-css-sort-attributes-block)
      ((kbd "C-k d") . com-css-sort-attributes-document)))

  ;; Eemmet
  (jcs-key emmet-mode-keymap
    `(((kbd "C-<return>") . jcs-emmet-expand-line))))
