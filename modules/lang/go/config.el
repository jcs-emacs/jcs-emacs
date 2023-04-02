;;; lang/go/config.el  -*- lexical-binding: t; -*-

(require 'cc-mode)

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

  (company-fuzzy-backend-add-before 'company-go 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]go")
                              'jcs-insert-go-template))

;;
;; (@* "Extensions" )
;;

(use-package company-go
  :init
  (setq company-go-show-annotation t))

(use-package flycheck-golangci-lint
  :hook (flycheck-mode . flycheck-golangci-lint-setup))
