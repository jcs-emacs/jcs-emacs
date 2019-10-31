;;; jcs-python-mode.el --- Python mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'python)
(require 'python-mode)


(defun jcs-python-mode-hook ()
  "Python mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]py")
                              'jcs-ask-python-template
                              t)

  ;; Normal
  (define-key python-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key python-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (define-key python-mode-map (kbd "<backspace>") #'jcs-smart-backspace)
  (define-key python-mode-map [C-backspace] #'jcs-backward-delete-word)

  (define-key python-mode-map [M-up] #'jcs-previous-blank-line)
  (define-key python-mode-map [M-down] #'jcs-next-blank-line)

  (define-key python-mode-map (kbd "C-k C-f") #'jcs-py-indent-region)
  (define-key python-mode-map (kbd "C-k C-d") #'jcs-py-format-document)
  (define-key python-mode-map (kbd "C-S-f") #'jcs-py-format-region-or-document)

  ;; Edit
  (define-key python-mode-map (kbd "<delete>") #'jcs-smart-delete)
  (define-key python-mode-map (kbd "TAB") #'jcs-tab-key)

  (define-key python-mode-map (kbd "RET") #'jcs-py-return)

  ;; Comment
  (define-key python-mode-map (kbd "\"") #'jcs-py-maybe-insert-codedoc))

(add-hook 'python-mode-hook 'jcs-python-mode-hook)


(provide 'jcs-python-mode)
;;; jcs-python-mode.el ends here
