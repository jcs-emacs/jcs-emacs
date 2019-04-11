;;; jcs-sql-mode.el --- SQL mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package sql-indent
  :config
  ;; URL(jenchieh): https://www.emacswiki.org/emacs/SqlIndent
  ;; 1 = 2 spaces,
  ;; 2 = 4 spaces,
  ;; 3 = 6 spaces,
  ;; n = n * 2 spaces,
  ;; etc.
  (setq sql-indent-offset 1))


(require 'sql)
(defun jcs-sql-mode-hook()
  "SQL mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (defun jcs-sql-format ()
    "File format for editing SQL file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-sql-template)))

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

(add-to-list 'auto-mode-alist '("\\.sql'?\\'" . sql-mode))


(provide 'jcs-sql-mode)
;;; jcs-sql-mode.el ends here
