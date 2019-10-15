;;; jcs-kotlin-mode.el --- Kotlin mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'kotlin-mode)


(defun jcs-kotlin-mode-hook ()
  "Kotlin mode hook."
  ;; File Header
  (jcs-insert-header-if-valid '("[.]kt"
                                "[.]ktm"
                                "[.]kts")
                              'jcs-insert-kotlin-template)
  )
(add-hook 'kotlin-mode-hook 'jcs-kotlin-mode-hook)


(provide 'jcs-kotlin-mode)
;;; jcs-kotlin-mode.el ends here
