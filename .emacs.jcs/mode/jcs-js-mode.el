;;; jcs-js-mode.el --- JavaScript mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'js2-mode)


(defun jcs-js-mode-hook ()
  "JavaScript mode hook."
  (impatient-mode t)

  (setq js2-bounce-indent-p t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]js")
                              'jcs-insert-js-template)

  ;; Normal
  (define-key js2-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key js2-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key js2-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key js2-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key js2-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'js2-mode-hook 'jcs-js-mode-hook)


(provide 'jcs-js-mode)
;;; jcs-js-mode.el ends here
