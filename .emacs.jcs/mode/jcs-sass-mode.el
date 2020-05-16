;;; jcs-sass-mode.el --- Sass mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'ssass-mode)

(defun jcs-sass-mode-hook ()
  "Sass mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sass")
                              'jcs-insert-sass-template)

  ;; Normal

  ;; Comment Block
  (define-key ssass-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key ssass-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; Edit
  (define-key ssass-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key ssass-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(add-hook 'ssass-mode-hook 'jcs-sass-mode-hook)

(provide 'jcs-sass-mode)
;;; jcs-sass-mode.el ends here
