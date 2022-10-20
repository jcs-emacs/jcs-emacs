;;; jcs-crystal-mode.el --- Crystal mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'crystal-mode)
(require 'flycheck-crystal)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-crystal-template "crystal" "default.txt"
  "Template for Crystal.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'crystal-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]cr")
                              'jcs-insert-crystal-template))

(provide 'jcs-crystal-mode)
;;; jcs-crystal-mode.el ends here
