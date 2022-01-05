;;; jcs-less-css-mode.el --- LESS CSS mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'less-css-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-less-template ()
  "Header for LESS header file."
  (jcs--file-header--insert "less" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'less-css-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]less")
                              'jcs-insert-less-template))

(provide 'jcs-less-css-mode)
;;; jcs-less-css-mode.el ends here
