;;; jcs-company.el --- Company related configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'company-box)

;;
;; (@* "Faces" )
;;

(defun jcs-company--theme (theme)
  "Update theme for `company'."
  (require 'asoc)
  (let (fg bg)
    (pcase theme
      (`vs-dark (setq bg "#2A2D38" fg "#F1F1F1"))
      (`vs-light (setq bg "#E9EAED" fg "#1E1E1E")))
    (asoc-put! company-box-doc-frame-parameters 'background-color bg t)
    (asoc-put! company-box-doc-frame-parameters 'foreground-color fg t)))

(jcs-theme-call #'jcs-company--theme)
(add-hook 'jcs-after-load-theme-hook #'jcs-company--theme)

;;
;; (@* "Util" )
;;

(defun jcs-company-safe-add-backend (backend)
  "Safe way to add backend."
  (if company-fuzzy-mode
      (progn
        (add-to-list 'company-fuzzy--backends backend t)
        (add-to-list 'company-fuzzy--recorded-backends backend t)
        (setq company-fuzzy--backends (delete-dups company-fuzzy--backends)
              company-fuzzy--recorded-backends (delete-dups company-fuzzy--recorded-backends)))
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends backend t)
    (setq company-backends (delete-dups company-backends))))

;;
;; (@* "Hooks" )
;;

(defun jcs--company-complete-selection--advice-around (fn)
  "Exection around `company-complete-selection' command."
  (let ((company-dabbrev-downcase t)) (call-interactively fn)))
(advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around)

(defun jcs-company--first-completion-started (_backend)
  "Run before company's completion once."
  (require 'yasnippet-snippets)
  (remove-hook 'company-completion-started-hook 'jcs-company--first-completion-started))
(add-hook 'company-completion-started-hook 'jcs-company--first-completion-started)

(defun jcs-company--completion-started (_backend)
  "Run before company's completion."
  (jcs-gc-cons-threshold-speed-up t))
(add-hook 'company-completion-started-hook 'jcs-company--completion-started)

(defun jcs-company--after-completion (&rest _)
  "Run after company's completion."
  (jcs-gc-cons-threshold-speed-up nil))
(add-hook 'company-after-completion-hook 'jcs-company--after-completion)

(provide 'jcs-company)
;;; jcs-company.el ends here
