;;; jcs-pascal-mode.el --- Pascal mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pascal)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-pascal-template ()
  "Header for Pascal header file."
  (jcs--file-header--insert "pascal" "default.txt"))

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
