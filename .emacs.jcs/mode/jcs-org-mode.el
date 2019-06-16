;;; jcs-org-mode.el --- Org mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'org)


;; No fold when open `org' file.
(setq org-startup-folded nil)


(defun jcs-org-mode-hook ()
  "Org mode hook."
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Normal
  (define-key org-mode-map (kbd "<up>") #'previous-line)
  (define-key org-mode-map (kbd "<down>") #'next-line)
  (define-key org-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key org-mode-map (kbd "C-c C-c") #'kill-ring-save)
  (define-key org-mode-map (kbd "C-a") #'jcs-mark-whole-buffer)
  (define-key org-mode-map [tab] #'jcs-tab-key)
  (define-key org-mode-map [C-tab] #'org-cycle)

  (define-key org-mode-map (kbd "C-y") #'jcs-redo)

  ;; `org-nav'
  (define-key org-mode-map (kbd "S-<up>") #'jcs-org-table-up)
  (define-key org-mode-map (kbd "S-<down>") #'jcs-org-table-down)
  (define-key org-mode-map (kbd "S-<left>") #'jcs-org-table-left)
  (define-key org-mode-map (kbd "S-<right>") #'jcs-org-table-right)
  )
(add-hook 'org-mode-hook 'jcs-org-mode-hook)


(provide 'jcs-org-mode)
;;; jcs-org-mode.el ends here
