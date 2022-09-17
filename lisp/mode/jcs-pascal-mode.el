;;; jcs-pascal-mode.el --- Pascal mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pascal)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-pascal-template "pascal" "default.txt"
  "Header for Pascal header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'pascal-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pas")
                              'jcs-insert-pascal-template))

(provide 'jcs-pascal-mode)
;;; jcs-pascal-mode.el ends here
