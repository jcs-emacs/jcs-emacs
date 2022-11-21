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
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  (company-fuzzy-backend-add 'company-ansible)

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

(use-package gitlab-ci-mode-flycheck
  :hook (flycheck-mode . gitlab-ci-mode-flycheck-enable))
