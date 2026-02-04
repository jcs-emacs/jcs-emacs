;;; lang/latex/config.el  -*- lexical-binding: t; -*-

(require 'tex)

;;
;; (@* "Settings" )
;;

(setq TeX-parse-self t ; parse on load
      TeX-auto-save t  ; parse on save
      ;; Use hidden directories for AUCTeX files.
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      ;; Don't start the Emacs server when correlating sources.
      TeX-source-correlate-start-server nil
      ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
      TeX-electric-sub-and-superscript t
      ;; Just save, don't ask before each compilation.
      TeX-save-query nil)

(msg-clean-add-echo-commands '(TeX-auto-list-information))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-latex-template "latex" "default.txt"
  "LaTex file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'LaTeX-mode-hook
  (let ((be-b 'company-dabbrev))
    ;; BibTeX
    (company-fuzzy-backend-add-before 'company-bibtex be-b)
    ;; AUCTeX
    (company-fuzzy-backend-add-before 'company-auctex-labels be-b)
    (company-fuzzy-backend-add-before 'company-auctex-bibs be-b)
    (company-fuzzy-backend-add-before 'company-auctex-macros be-b)
    (company-fuzzy-backend-add-before 'company-auctex-symbols be-b)
    (company-fuzzy-backend-add-before 'company-auctex-environments be-b)
    ;; RefTeX
    (company-fuzzy-backend-add-before 'company-reftex-labels be-b)
    (company-fuzzy-backend-add-before 'company-reftex-citations be-b)
    ;; LaTex
    (company-fuzzy-backend-add-before 'company-latex-commands be-b)
    (company-fuzzy-backend-add-before 'company-math-symbols-latex be-b)
    (company-fuzzy-backend-add-before 'company-math-symbols-unicode be-b))

  ;; File Header
  (jcs-insert-header-if-valid '("[.]tex")
                              'jcs-insert-latex-template))
