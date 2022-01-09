;;; jcs-git-mode.el --- Git related modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'git-modes)

(require 'gitignore-templates)

(defun jcs-gitignore--ask-template (name)
  "Ask for inserting .gitignore template for NAME."
  (interactive
   (list (completing-read ".gitignore template: "
                          (append (list "Empty (Default)")
                                  (gitignore-templates-names))
                          nil t)))
  (pcase name
    ("Empty (Default)" )
    (_ (insert (gitignore-templates name))))
  (message "[INFO] Insert template `%s`" name))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'gitattributes-mode-hook
  (electric-pair-mode nil)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next)))))

(jcs-add-hook 'gitconfig-mode-hook
  (electric-pair-mode nil)

  (jcs-key-local
    `(((kbd "<up>")    . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>")  . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "C-d")     . jcs-kill-whole-line))))

(jcs-add-hook 'gitignore-mode-hook
  (electric-pair-mode nil)

  (jcs-insert-header-if-valid '("[.]gitignore")
                              'jcs-gitignore--ask-template
                              :interactive t
                              :success
                              (lambda ()
                                (when (jcs-current-line-empty-p)
                                  (jcs-kill-whole-line))))

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "C-d")    . jcs-kill-whole-line))))

(provide 'jcs-git-mode)
;;; jcs-git-mode.el ends here
