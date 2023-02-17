;;; lang/emacs-lisp/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-emacs-lisp-template "elisp" "default.txt"
  "Template for Emacs Lisp.")

(file-header-defins jcs-insert-lisp-template "lisp" "default.txt"
  "Lisp file header format.")

;;
;; (@* "Hooks" )
;;

(jcs-add-hook 'emacs-lisp-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.

  (unless (equal (buffer-name) dir-locals-file)
    (jcs-insert-header-if-valid '("[.]el")
                                'jcs-insert-emacs-lisp-template))

  (company-fuzzy-backend-add 'company-elisp-keywords)

  (eask-api-setup))

(jcs-add-hook 'emacs-lisp-compilation-mode-hook
  (setq truncate-lines t))

(jcs-add-hook 'lisp-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word.
  (jcs-insert-header-if-valid '("[.]lisp")
                              'jcs-insert-lisp-template))

(jcs-add-hook 'lisp-interaction-mode-hook
  (jcs-key-local
    `(((kbd "M-K") . jcs-scratch-buffer-refresh))))

(jcs-add-hook 'eask-mode-hook
  (company-fuzzy-backend-add-before 'company-eask 'company-dabbrev)
  (eldoc-eask-enable))

;;
;; (@* "Extensions" )
;;

(use-package elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package flycheck-cask    :hook (flycheck-mode . flycheck-cask-setup))
(use-package flycheck-eask    :hook (flycheck-mode . flycheck-eask-setup))

(use-package flycheck-elsa
  :hook (flycheck-mode . flycheck-elsa-setup)
  :init
  (setq flycheck-elsa-backend 'eask))

(use-package flycheck-package :hook (flycheck-mode . flycheck-package-setup))
(use-package flycheck-relint  :hook (flycheck-mode . flycheck-relint-setup))
