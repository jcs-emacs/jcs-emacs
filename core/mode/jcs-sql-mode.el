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

(jcs-add-hook 'sql-mode-hook
  (jcs-elec-pair-add '((?\` . ?\`)))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sql")
                              'jcs-insert-sql-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(provide 'jcs-sql-mode)
;;; jcs-sql-mode.el ends here
