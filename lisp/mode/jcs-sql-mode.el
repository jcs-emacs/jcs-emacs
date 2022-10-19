;;; jcs-sql-mode.el --- SQL mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'sql)
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

(leaf sql-indent
  :init
  ;; URL: https://www.emacswiki.org/emacs/SqlIndent

  ;; 1 = 2 spaces,
  ;; 2 = 4 spaces,
  ;; 3 = 6 spaces,
  ;; n = n * 2 spaces,
  ;; etc.
  (setq sql-indent-offset 1))

(provide 'jcs-sql-mode)
;;; jcs-sql-mode.el ends here
