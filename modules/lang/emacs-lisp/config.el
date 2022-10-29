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
  (jcs-insert-header-if-valid '("[.]el")
                              'jcs-insert-emacs-lisp-template)
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

;;
;; (@* "Extensions" )
;;

(leaf elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(leaf flycheck-eask           :hook (flycheck-mode-hook . flycheck-eask-setup))
(leaf flycheck-elsa           :hook (flycheck-mode-hook . flycheck-elsa-setup))
(leaf flycheck-package        :hook (flycheck-mode-hook . flycheck-package-setup))
(leaf flycheck-relint         :hook (flycheck-mode-hook . flycheck-relint-setup))
