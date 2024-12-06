;;; lang/scheme/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-scheme-template "scheme" "default.txt"
  "Header for Scheme header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'scheme-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]scm" "[.]ss")
                              'jcs-insert-scheme-template))

;;
;; (@* "Extensions" )
;;

(use-package geiser
  :init
  (setq geiser-autodoc-identifier-format "%s → %s"
        geiser-repl-per-project-p t))

(use-package flymake-guile :hook (flymake-mode . flymake-guile))
