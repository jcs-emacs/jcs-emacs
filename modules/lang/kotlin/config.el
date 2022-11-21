;;; lang/kotlin/config.el  -*- lexical-binding: t; -*-

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

(use-package flycheck-kotlin
  :hook (flycheck-mode . flycheck-kotlin-setup))
