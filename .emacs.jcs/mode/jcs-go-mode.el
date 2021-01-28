;;; jcs-go-mode.el --- GO mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'go-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-go-mode-hook ()
  "Go mode hook."
  (jcs-use-cc-mutliline-comment)

  (setq-local docstr-show-type-name nil)

  (face-remap-add-relative 'font-lock-constant-face '(:inherit jcs-font-lock-null-face))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]go")
                              'jcs-insert-go-template))

(add-hook 'go-mode-hook 'jcs-go-mode-hook)

(provide 'jcs-go-mode)
;;; jcs-go-mode.el ends here
