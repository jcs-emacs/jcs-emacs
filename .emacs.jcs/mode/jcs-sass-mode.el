;;; jcs-sass-mode.el --- Sass mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'ssass-mode)


(defun jcs-sass-mode-hook ()
  "Sass mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)
  (lsp-deferred)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sass")
                              'jcs-insert-sass-template)

  ;; Normal

  ;; Comment Block
  (define-key ssass-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key ssass-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; Edit
  (define-key ssass-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key ssass-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'ssass-mode-hook 'jcs-sass-mode-hook)


(provide 'jcs-sass-mode)
;;; jcs-sass-mode.el ends here
