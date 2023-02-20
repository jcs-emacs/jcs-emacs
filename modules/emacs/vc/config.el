;;; emacs/vc/config.el  -*- lexical-binding: t; -*-

(require 'git-modes)
(require 'gitignore-templates)

;;
;; (@* "Templates" )
;;

(file-header-defsrc jcs-gitignore--ask-template ".gitignore template: "
  (append (list "Empty (Default)") (gitignore-templates-names))
  (pcase index
    (0 )
    (_ (insert (gitignore-templates source))))
  (message "[INFO] Insert template `%s`" name))

;;
;; (@* "Hook" )
;;

(jcs-add-hook '(gitattributes-mode-hook gitconfig-mode-hook gitignore-mode-hook)
  (jcs-key-local
    `(((kbd "C-d") . jcs-kill-whole-line))))

(jcs-add-hook 'gitignore-mode-hook
  (jcs-insert-header-if-valid '("[.]gitignore")
                              'jcs-gitignore--ask-template
                              :interactive t
                              :success
                              (lambda ()
                                (when (jcs-current-line-empty-p)
                                  (jcs-kill-whole-line)))))

;;
;; (@* "Extensions" )
;;

(use-package vc-refresh
  :init
  (setq vc-refresh-commands '( magit-checkout
                               magit-branch-and-checkout
                               magit-branch-or-checkout
                               magit-branch-checkout)))
