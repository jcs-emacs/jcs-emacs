;;; lang/emacs-lisp/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Comment" )
;;

(defun jcs-use-lisp-comment ()
  "Fixed lisp comment."
  (setq-local comment-start-skip
              (with-temp-buffer (funcall #'lisp-mode) comment-start-skip)
              comment-end-skip
              (with-temp-buffer (funcall #'lisp-mode) comment-end-skip)))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-emacs-lisp-dir-locals-template "elisp" "dir-locals.txt"
  "Template for Emacs Lisp for .dir-locals file.")

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
                              (if (equal (buffer-name) dir-locals-file)
                                  'jcs-insert-emacs-lisp-dir-locals-template
                                'jcs-insert-emacs-lisp-template))

  (company-fuzzy-backend-add-before 'company-elisp-keywords 'company-dabbrev)

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
