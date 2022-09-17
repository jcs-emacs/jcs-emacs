;;; jcs-scss-mode.el --- SCSS mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'scss-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-scss-template "scss" "default.txt"
  "Header for SCSS header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'scss-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]scss")
                              'jcs-insert-scss-template)

  (jcs-key-local
    `(((kbd "C-k s") . com-css-sort-attributes-block)
      ((kbd "C-k d") . com-css-sort-attributes-document))))

(provide 'jcs-scss-mode)
;;; jcs-scss-mode.el ends here
