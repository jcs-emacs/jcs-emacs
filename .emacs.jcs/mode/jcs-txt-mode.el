;;; jcs-txt-mode.el --- Text related modes. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'gitignore-mode)
(defun jcs-gitignore-mode-hook ()
  "Gitignore mode hook."
  (electric-pair-mode nil)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Normal
  (define-key gitignore-mode-map (kbd "<up>") 'previous-line)
  (define-key gitignore-mode-map (kbd "<down>") 'next-line)
  (define-key gitignore-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key gitignore-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key gitignore-mode-map (kbd "<up>") 'previous-line)
  (define-key gitignore-mode-map (kbd "<down>") 'next-line)
  )
(add-hook 'gitignore-mode-hook 'jcs-gitignore-mode-hook)



(require 'gitattributes-mode)
(defun jcs-gitattributes-mode-hook ()
  "Gitattributes mode hook."
  (electric-pair-mode nil)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Normal
  (define-key gitattributes-mode-map (kbd "<up>") 'previous-line)
  (define-key gitattributes-mode-map (kbd "<down>") 'next-line)
  (define-key gitattributes-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key gitattributes-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key gitattributes-mode-map (kbd "<up>") 'previous-line)
  (define-key gitattributes-mode-map (kbd "<down>") 'next-line)
  )
(add-hook 'gitattributes-mode-hook 'jcs-gitattributes-mode-hook)



(require 'org)
;; No fold when open `org' file.
(setq org-startup-folded nil)

(defun jcs-org-mode-hook ()
  "Org mode hook."
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (defun jcs-org-mode-format()
    "Fromat the given file as a text file related. - JenChieh Text file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-txt-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]txt" buffer-file-name) (jcs-org-mode-format))
          ))

  ;; Normal
  (define-key org-mode-map (kbd "<up>") #'previous-line)
  (define-key org-mode-map (kbd "<down>") #'next-line)
  (define-key org-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key org-mode-map "\C-c\C-c" #'kill-ring-save)
  (define-key org-mode-map "\C-a" #'mark-whole-buffer)
  (define-key org-mode-map [tab] #'jcs-tab-key)
  (define-key org-mode-map [C-tab] #'org-cycle)

  (define-key org-mode-map "\C-y" #'jcs-redo)

  ;; `org-nav'
  (define-key org-mode-map (kbd "S-<up>") #'jcs-org-table-up)
  (define-key org-mode-map (kbd "S-<down>") #'jcs-org-table-down)
  (define-key org-mode-map (kbd "S-<left>") #'jcs-org-table-left)
  (define-key org-mode-map (kbd "S-<right>") #'jcs-org-table-right)
  )
(add-hook 'org-mode-hook 'jcs-org-mode-hook)


(provide 'jcs-txt-mode)
;;; jcs-txt-mode.el ends here
