;;; completion/company/config.el  -*- lexical-binding: t; -*-

(use-package company
  :bind ( :map company-active-map
          ([tab] . vsc-edit-tab)
          ("TAB" . vsc-edit-tab)
          ("C-s" . jcs-save-buffer))
  :init
  (setq company-minimum-prefix-length 0
        company-tooltip-limit 10
        company-idle-delay 0.07
        company-selection-wrap-around 'on
        company-require-match nil
        company-tooltip-align-annotations t
        company-format-margin-function #'company-detect-icons-margin
        company-frontends '(company-pseudo-tooltip-frontend)
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-eclim-auto-save nil)
  (setq company-backends
        '( company-capf company-semantic
           company-keywords company-dict
           company-abbrev company-dabbrev company-dabbrev-code
           company-paths
           company-etags company-gtags
           company-yasnippet))
  :config
  (unless elenv-graphic-p
    (push 'company-echo-metadata-frontend company-frontends))

  ;; XXX: The variable `company-continue-commands' is in the not clause;
  ;; therefore, these are commands that are not continuable.
  (elenv-uappend company-continue-commands
    '( kill-region
       whole-line-or-region-kill-region)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-backends-colors nil
        company-box-frame-behavior 'point
        company-box-scrollbar 'right
        company-box-doc-delay 0.3
        company-box-doc-text-scale-level -2))

(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@" "::" ":")
        company-fuzzy-reset-selection t))

(use-package company-files
  :init
  (setq company-files-chop-trailing-slash nil))

(use-package company-paths
  :init
  (setq company-paths-continue-completing t))

(use-package company-emojify
  :init
  (setq company-emojify-annotation (if elenv-graphic-p 'image 'unicode)
        company-emojify-emoji-styles '(github)))

;;
;; (@* "Faces" )
;;

(defun jcs-company--theme (theme)
  "Update theme for `company'."
  (jcs-require '(asoc company-box))
  (let (fg bg)
    (pcase theme
      (`vs-dark  (setq bg "#2A2D38" fg "#F1F1F1"))
      (`vs-light (setq bg "#E9EAED" fg "#1E1E1E")))
    (asoc-put! company-box-doc-frame-parameters 'background-color bg t)
    (asoc-put! company-box-doc-frame-parameters 'foreground-color fg t)))

(jcs-theme-call #'jcs-company--theme)
(add-hook 'jcs-after-load-theme-hook #'jcs-company--theme)

;;
;; (@* "Hooks" )
;;

(jcs-advice-add 'company-complete-selection :around
  (let ((company-dabbrev-downcase t)) (apply arg0 args)))
