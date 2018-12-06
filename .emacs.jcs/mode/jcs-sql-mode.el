;; ========================================================================
;; $File: jcs-sql-mode.el $
;; $Date: 2017-09-25 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh SQL mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'sql-indent)
;; URL(jenchieh): https://www.emacswiki.org/emacs/SqlIndent
;; 1 = 2 spaces,
;; 2 = 4 spaces,
;; 3 = 6 spaces,
;; n = n * 2 spaces,
;; etc.
(setq sql-indent-offset 1)

(require 'sql)
(defun jcs-sql-mode-hook()
  "Add hooks to `sql-mode' hook."
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; turn on auto complete.
  (auto-complete-mode t)

  (defun jcs-sql-format ()
    "File format for editing SQL file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-sql-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]sql" buffer-file-name) (jcs-sql-format))
          ))

  ;; Edit
  (define-key sql-mode-map (kbd "<up>") #'jcs-smart-indent-up)
  (define-key sql-mode-map (kbd "<down>") #'jcs-smart-indent-down)

  (define-key sql-mode-map "\C-c\C-c" 'kill-ring-save)
  )

(add-hook 'sql-mode-hook 'jcs-sql-mode-hook)

(add-to-list 'auto-mode-alist '("\\.sql?\\'" . sql-mode))
