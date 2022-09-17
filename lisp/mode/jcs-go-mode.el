;;; jcs-go-mode.el --- GO mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'go-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-go-template "go" "default.txt"
  "Header for Go header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'go-mode-hook
  (jcs-use-cc-mutliline-comment)

  (setq-local docstr-show-type-name nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]go")
                              'jcs-insert-go-template))

(provide 'jcs-go-mode)
;;; jcs-go-mode.el ends here
