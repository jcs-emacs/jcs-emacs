;;; jcs-crystal-mode.el --- Crystal mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'crystal-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-crystal-template ()
  "Template for Crystal."
  (jcs--file-header--insert "crystal" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'crystal-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]cr")
                              'jcs-insert-crystal-template))

(provide 'jcs-crystal-mode)
;;; jcs-crystal-mode.el ends here
