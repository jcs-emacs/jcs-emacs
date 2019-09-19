;;; jcs-org-mode.el --- Org mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'org)
(require 'org-bullets)

(add-hook 'org-mode-hook 'org-bullets-mode)


(setq org-todo-keywords
      '((sequence "TODO"
                  "WAITING"
                  "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red")
        ("WAITING" :foreground "yellow")
        ("DONE" :foreground "green")))


(defun jcs-org-mode-hook ()
  "Org mode hook."
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Normal
  (define-key org-mode-map (kbd "C-a") #'jcs-mark-whole-buffer)
  (define-key org-mode-map [tab] #'jcs-tab-key)

  (define-key org-mode-map (kbd "C-y") #'jcs-redo)

  (define-key org-mode-map (kbd "C-s") #'save-buffer)

  (define-key org-mode-map [S-tab] #'org-cycle)
  (define-key org-mode-map (kbd "C-k") nil)
  (define-key org-mode-map (kbd "C-<return>") #'jcs-ctrl-return-key)

  ;; `org-nav'
  (define-key org-mode-map (kbd "S-<up>") #'jcs-org-table-up)
  (define-key org-mode-map (kbd "S-<down>") #'jcs-org-table-down)
  (define-key org-mode-map (kbd "S-<left>") #'jcs-org-table-left)
  (define-key org-mode-map (kbd "S-<right>") #'jcs-org-table-right)
  )
(add-hook 'org-mode-hook 'jcs-org-mode-hook)


(provide 'jcs-org-mode)
;;; jcs-org-mode.el ends here
