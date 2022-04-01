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
;; (@* "Hooks" )
;;

(defun jcs--company-complete-selection--advice-around (fnc &rest args)
  "Exection around `company-complete-selection' command."
  (let ((company-dabbrev-downcase t)) (apply fnc args)))
(advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around)

(jcs-add-hook 'company-completion-started-hook (jcs-gc-cons-threshold-speed-up t))
(jcs-add-hook 'company-after-completion-hook (jcs-gc-cons-threshold-speed-up nil))

(provide 'jcs-company)
;;; jcs-company.el ends here
