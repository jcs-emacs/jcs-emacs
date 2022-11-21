;;; lang/sql/config.el  -*- lexical-binding: t; -*-

(require 'sql-indent)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-sql-template "sql" "default.txt"
  "Header for SQL header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'sql-mode-hook
  (jcs-elec-pair-add '((?\` . ?\`)))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sql")
                              'jcs-insert-sql-template))

;;
;; (@* "Extensions" )
;;

(use-package sql-indent
  :init
  ;; URL: https://www.emacswiki.org/emacs/SqlIndent

  ;; 1 = 2 spaces,
  ;; 2 = 4 spaces,
  ;; 3 = 6 spaces,
  ;; n = n * 2 spaces,
  ;; etc.
  (setq sql-indent-offset 1))
