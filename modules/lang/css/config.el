;;; lang/css/config.el  -*- lexical-binding: t; -*-

(require 'css-mode)
(require 'web-mode)
(require 'lsp-tailwindcss)

;;
;; (@* "Macro" )
;;

(defmacro jcs-css-add-hook (&rest body)
  "Add hook for all css related modes."
  (declare (indent 0))
  `(dolist (mode lsp-tailwindcss-major-modes)
     (jcs-add-hook (jcs-as-hook mode) ,@body)))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-css-template "css" "default.txt"
  "Template for CSS.")

;;
;; (@* "Hook" )
;;

(add-hook 'css-mode-hook 'emmet-mode)

(jcs-add-hook 'css-mode-hook
  (impatient-mode t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]css")
                              'jcs-insert-css-template)

  (jcs-key-local
    `(((kbd "C-k s") . com-css-sort-attributes-block)
      ((kbd "C-k d") . com-css-sort-attributes-document)))

  ;; Eemmet
  (jcs-key emmet-mode-keymap
    `(((kbd "C-<return>") . jcs-emmet-expand-line))))

;;
;; (@* "Extensions" )
;;

(use-package css-eldoc
  :init
  (css-eldoc-enable))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-emmet-completions t))

(use-package company-tailwindcss
  :init
  (jcs-css-add-hook
    (company-fuzzy-backend-add-before 'company-tailwindcss 'company-dabbrev)))

(use-package company-bootstrap
  :init
  (setq company-tailwindcss-complete-only-in-attributes t
        company-tailwindcss-sort-post-completion nil)
  (jcs-css-add-hook
    (company-fuzzy-backend-add-before 'company-bootstrap 'company-dabbrev)))
