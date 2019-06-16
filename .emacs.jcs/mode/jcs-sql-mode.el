;;; jcs-sql-mode.el --- SQL mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'sql)
(require 'sql-indent)


(defun jcs-sql-format ()
  "File format for editing SQL file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-sql-template)))


(defun jcs-sql-mode-hook()
  "SQL mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]sql" buffer-file-name) (jcs-sql-format))
          ))

  ;; Normal
  (define-key sql-mode-map (kbd "<up>") #'jcs-smart-indent-up)
  (define-key sql-mode-map (kbd "<down>") #'jcs-smart-indent-down)

  (define-key sql-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'sql-mode-hook 'jcs-sql-mode-hook)


(provide 'jcs-sql-mode)
;;; jcs-sql-mode.el ends here
