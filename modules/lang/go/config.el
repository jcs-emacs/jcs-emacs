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

  (company-fuzzy-backend-add 'company-go)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]go")
                              'jcs-insert-go-template))

;;
;; (@* "Extensions" )
;;

(leaf company-go
  :init
  (setq company-go-show-annotation t))

(leaf flycheck-golangci-lint
  :hook (flycheck-mode-hook . flycheck-golangci-lint-setup))
