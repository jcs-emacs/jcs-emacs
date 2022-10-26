;;; jcs-kotlin-mode.el --- Kotlin mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'kotlin-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-kotlin-template "kotlin" "default.txt"
  "Header for Kotlin header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'kotlin-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]kt"
                                "[.]ktm"
                                "[.]kts")
                              'jcs-insert-kotlin-template))

;;
;; (@* "Extensions" )
;;

(leaf flycheck-kotlin
  :hook (flycheck-mode-hook . flycheck-kotlin-setup))

(provide 'jcs-kotlin-mode)
;;; jcs-kotlin-mode.el ends here
