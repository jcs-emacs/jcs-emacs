;;; jcs-git-mode.el --- Git related modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'git-modes)

(require 'gitignore-templates)

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

(provide 'jcs-git-mode)
;;; jcs-git-mode.el ends here
