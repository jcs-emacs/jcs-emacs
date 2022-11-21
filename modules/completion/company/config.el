;;; completion/company/config.el  -*- lexical-binding: t; -*-

(use-package company
  :init
  (setq company-frontends '(company-pseudo-tooltip-frontend)
        company-require-match nil
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-eclim-auto-save nil
        company-minimum-prefix-length 0
        company-idle-delay 0.07
        company-selection-wrap-around 'on
        company-format-margin-function #'company-detect-icons-margin)
  (setq company-backends
        '( company-capf company-semantic
           company-keywords
           company-abbrev company-dabbrev company-dabbrev-code
           company-files
           company-etags company-gtags
           company-yasnippet))
  :config
  (unless elenv-graphic-p
    (push 'company-echo-metadata-frontend company-frontends)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-backends-colors nil
        company-box-frame-behavior 'point
        company-box-scrollbar 'right
        company-box-doc-delay 0.3
        company-box-doc-text-scale-level -2))

(use-package company-emojify
  :init
  (setq company-emojify-annotation (if elenv-graphic-p 'image 'unicode)
        company-emojify-emoji-styles '(github)))

(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))

;;
;; (@* "Faces" )
;;

(defun jcs-company--theme (theme)
  "Update theme for `company'."
  (jcs-require '(asoc company-box))
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

(jcs-advice-add 'company-complete-selection :around
  (let ((company-dabbrev-downcase t)) (apply arg0 args)))
