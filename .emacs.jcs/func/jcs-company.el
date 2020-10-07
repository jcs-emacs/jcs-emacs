;;; jcs-company.el --- Company related configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(custom-set-faces
 ;;--------------------------------------------------------------------
 ;; Preview
 '(company-preview ((t (:foreground "dark gray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 ;;--------------------------------------------------------------------
 ;; Base Selection
 '(company-tooltip ((t (:background "light gray" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white"))))
 ;;--------------------------------------------------------------------
 ;; Keyword Selection
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:background "light gray" :foreground "#C00000"))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:background "steel blue" :foreground "#C00000"))))
 ;;--------------------------------------------------------------------
 ;; Scroll Bar
 '(company-scrollbar-fg ((t (:background "black"))))
 '(company-scrollbar-bg ((t (:background "dark gray")))))

(defun jcs-company-safe-add-backend (backend)
  "Safe way to add backend."
  (if company-fuzzy-mode
      (progn
        (add-to-list 'company-fuzzy--backends backend)
        (add-to-list 'company-fuzzy--recorded-backends backend)
        (setq company-fuzzy--backends (delete-dups company-fuzzy--backends)
              company-fuzzy--recorded-backends (delete-dups company-fuzzy--recorded-backends)))
    (add-to-list 'company-backends backend)
    (setq company-backends (delete-dups company-backends))))

(defun jcs--company-complete-selection--advice-around (fn)
  "Advice execute around `company-complete-selection' command."
  (let ((company-dabbrev-downcase t)) (call-interactively fn)))
(advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around)

(defun jcs--company-completion-started-hook (_backend)
  "Hook bind to `company-completion-started-hook'."
  (jcs-gc-cons-threshold-speed-up t)
  (require 'yasnippet-snippets))
(add-hook 'company-completion-started-hook 'jcs--company-completion-started-hook)

(defun jcs--company-after-completion-hook (&rest _)
  "Hook bind to `company-after-completion-hook'."
  (jcs-gc-cons-threshold-speed-up nil))
(add-hook 'company-after-completion-hook 'jcs--company-after-completion-hook)

(provide 'jcs-company)
;;; jcs-company.el ends here
