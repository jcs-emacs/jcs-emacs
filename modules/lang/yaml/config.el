;;; lang/yaml/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-yaml-template "yaml" "default.txt"
  "Header for YAML header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'yaml-mode-hook
  (company-fuzzy-backend-add-before 'company-ansible 'company-dabbrev)

  (flymake-ansible-lint-setup)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]yaml"
                                "[.]yml")
                              'jcs-insert-yaml-template)

  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-actionlint
  :hook (flycheck-mode . flycheck-actionlint-setup))

(use-package gitlab-ci-mode-flycheck
  :hook (flycheck-mode . gitlab-ci-mode-flycheck-enable))

(use-package flymake-yamllint
  :hook (yaml-mode . flymake-yamllint-setup))
