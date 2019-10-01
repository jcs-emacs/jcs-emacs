;;; jcs-go-mode.el --- GO mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'go-mode)


(defun jcs-go-mode-hook ()
  "Go mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]go")
                              'jcs-insert-go-template)

  )
(add-hook 'go-mode-hook 'jcs-go-mode-hook)


(provide 'jcs-go-mode)
;;; jcs-go-mode.el ends here
