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
                              'jcs-insert-sql-template)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next)))))

(provide 'jcs-sql-mode)
;;; jcs-sql-mode.el ends here
