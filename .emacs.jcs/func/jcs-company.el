;;; jcs-company.el --- Company related configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'company-box)

;;
;; (@* "Faces" )
;;

(defcustom jcs-company-theme (if (display-graphic-p) 'vs 'auto-complete)
  "Company theem style."
  :type '(choice (const :tag "Auto-Complete" auto-complete)
                 (const :tag "Visual Studio IDE" vs))
  :group 'jcs)

(cl-defun jcs-company-set-face
    (&key annotation tooltip scrollbar quickhelp
          tooltip-selection tooltip-common tooltip-common-selection)
  "Set face for `company-mode'."
  (custom-set-faces
   ;; --- Annotation
   `(company-tooltip-annotation ((t (:foreground ,(car annotation)))))
   `(company-fuzzy-annotation-face ((t (:foreground ,(cdr annotation)))))
   ;; --- Preview
   `(company-preview ((t (:foreground "dark gray" :underline t))))
   `(company-preview-common ((t (:inherit company-preview))))
   ;; --- Base Selection
   `(company-tooltip ((t (:background ,(car tooltip) :foreground ,(cdr tooltip)))))
   `(company-tooltip-selection ((t (:background ,(car tooltip-selection) :foreground ,(cdr tooltip-selection)))))
   ;; --- Keyword Selection
   `(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:background ,(car tooltip-common) :foreground ,(cdr tooltip-common)))))
   `(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:background ,(car tooltip-common-selection) :foreground ,(cdr tooltip-common-selection)))))
   ;; --- Scroll Bar
   `(company-scrollbar-bg ((t (:background ,(car scrollbar)))))
   `(company-scrollbar-fg ((t (:background ,(cdr scrollbar)))))
   ;; --- Tooltip
   `(popup-tip-face ((t (:background ,(car quickhelp) :foreground ,(cdr quickhelp))))))
  (setq company-box-doc-frame-parameters
        (jcs-set-alist company-box-doc-frame-parameters 'background-color (car quickhelp))
        company-box-doc-frame-parameters
        (jcs-set-alist company-box-doc-frame-parameters 'foreground-color (cdr quickhelp))))

(defun jcs-company-auto-complete-theme ()
  "Auto-Complete theme for `company-mode'."
  (jcs-company-set-face
   :annotation '("red4" . "red4")
   :tooltip '("light gray" . "black")
   :tooltip-selection '("steel blue" . "white")
   :tooltip-common '("light gray" . "#C00000")
   :tooltip-common-selection '("steel blue" . "#C00000")
   :scrollbar '("dark gray" . "black")
   :quickhelp '("#FFF08A" . "black")))

(defun jcs-company-vs-theme ()
  "Auto-Complete theme for `company-mode'."
  (if (jcs-is-light-theme-p)
      (jcs-company-set-face
       :annotation '("#41474D" . "#5E85AB")
       :tooltip '("#F5F5F5" . "black")
       :tooltip-selection '("#D6EBFF" . "black")
       :tooltip-common '("#F5F5F5" . "#0066BF")
       :tooltip-common-selection '("#D6EBFF" . "#0066BF")
       :scrollbar '("#F5F5F5" . "#C2C3C9")
       :quickhelp '("#E9EAED" . "#1E1E1E"))
    (jcs-company-set-face
     :annotation '("#96A2AA" . "#7BABCA")
     :tooltip '("#252526" . "#BEBEBF")
     :tooltip-selection '("#062F4A" . "#BEBEBF")
     :tooltip-common '("#252526" . "#0096FA")
     :tooltip-common-selection '("#062F4A" . "#0096FA")
     :scrollbar '("#3E3E42" . "#686868")
     :quickhelp '("#2A2D38" . "#F1F1F1"))))

(defun jcs-company-default-theme ()
  "Default theme for `company-mode'."
  (cl-case jcs-company-theme
    (auto-complete (jcs-company-auto-complete-theme))
    (vs (jcs-company-vs-theme))
    (t (user-error "Unknown `company-mode` theme type: %s" jcs-company-theme))))

(jcs-company-default-theme)

;;
;; (@* "Util" )
;;

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

;;
;; (@* "Hooks" )
;;

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
