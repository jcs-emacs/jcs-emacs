;;; jcs-org-mode.el --- Org mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 's)

(require 'org)
(require 'org-bullets)

(add-hook 'org-mode-hook 'org-bullets-mode)

(setq org-startup-folded nil)

(setq org-todo-keywords '((sequence "TODO" "WAITING" "DONE"))
      org-todo-keyword-faces '(("TODO" :foreground "red")
                               ("WAITING" :foreground "yellow")
                               ("DONE" :foreground "green"))
      org-log-done 'time)

;;
;; (@* "Hook" )
;;

(defun jcs-org-mode-hook ()
  "Org mode hook."
  ;; Normal
  (jcs-bind-key (kbd "C-a") #'jcs-mark-whole-buffer)
  (jcs-bind-key [tab] #'jcs-tab-key)

  (jcs-bind-key (kbd "C-y") #'jcs-redo)

  (jcs-bind-key [S-tab] #'jcs-org-smart-cycle)
  (jcs-bind-key (kbd "C-k") nil)
  (jcs-bind-key (kbd "C-<return>") #'jcs-ctrl-return-key)

  ;; `org-nav'
  (jcs-bind-key (kbd "S-<up>") #'jcs-org-table-up)
  (jcs-bind-key (kbd "S-<down>") #'jcs-org-table-down)
  (jcs-bind-key (kbd "S-<left>") #'jcs-org-table-left)
  (jcs-bind-key (kbd "S-<right>") #'jcs-org-table-right))

(add-hook 'org-mode-hook 'jcs-org-mode-hook)

(provide 'jcs-org-mode)
;;; jcs-org-mode.el ends here
