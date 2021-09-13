;;; jcs-python-mode.el --- Python mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'python)
(require 'python-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-python-mode-hook ()
  "Python mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]py")
                              'jcs-ask-python-template
                              :interactive t)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "<backspace>") #'jcs-smart-backspace)
  (jcs-bind-key [C-backspace] #'jcs-backward-delete-word)

  (jcs-bind-key [M-up] #'jcs-previous-blank-line)
  (jcs-bind-key [M-down] #'jcs-next-blank-line)

  (jcs-bind-key (kbd "C-k C-f") #'jcs-py-indent-region)
  (jcs-bind-key (kbd "C-k C-d") #'jcs-py-format-document)
  (jcs-bind-key (kbd "C-S-f") #'jcs-py-format-region-or-document)

  ;; Edit
  (jcs-bind-key (kbd "<delete>") #'jcs-smart-delete)
  (jcs-bind-key (kbd "TAB") #'jcs-tab-key)

  (jcs-bind-key (kbd "RET") #'jcs-py-return)
  (jcs-bind-key (kbd "C-v") #'yank))

(add-hook 'python-mode-hook 'jcs-python-mode-hook)

(provide 'jcs-python-mode)
;;; jcs-python-mode.el ends here
