;;; jcs-sql-mode.el --- SQL mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'sql)
(require 'sql-indent)

(defun jcs-sql-mode-hook()
  "SQL mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sql")
                              'jcs-insert-sql-template)

  ;; Normal
  (define-key sql-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key sql-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(add-hook 'sql-mode-hook 'jcs-sql-mode-hook)

(provide 'jcs-sql-mode)
;;; jcs-sql-mode.el ends here
