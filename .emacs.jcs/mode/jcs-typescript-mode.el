;;; jcs-typescript-mode.el --- TypeScript mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'typescript-mode)


(defun jcs-typescript-mode-hook ()
  "TypeScript mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]ts")
                              'jcs-insert-typescript-template)

  ;; Normal
  (define-key typescript-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key typescript-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key typescript-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key typescript-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key typescript-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'typescript-mode-hook 'jcs-typescript-mode-hook)


(provide 'jcs-typescript-mode)
;;; jcs-typescript-mode.el ends here
