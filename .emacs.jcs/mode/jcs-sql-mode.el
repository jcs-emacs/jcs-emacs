;;; jcs-sql-mode.el --- SQL mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'sql)
(require 'sql-indent)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-sql-template ()
  "Header for SQL header file."
  (jcs--file-header--insert "sql" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-sql-mode-hook()
  "SQL mode hook."
  (jcs-make-electric-pair-pairs-local '((?\` . ?\`)))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sql")
                              'jcs-insert-sql-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(add-hook 'sql-mode-hook 'jcs-sql-mode-hook)

(provide 'jcs-sql-mode)
;;; jcs-sql-mode.el ends here
