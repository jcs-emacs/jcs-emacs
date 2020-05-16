;;; jcs-typescript-mode.el --- TypeScript mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'typescript-mode)

(defun jcs-typescript-mode-hook ()
  "TypeScript mode hook."

  (face-remap-add-relative 'typescript-jsdoc-tag '(:inherit jcs-oop-tag-face))
  (face-remap-add-relative 'typescript-jsdoc-type '(:inherit jcs-oop-type-face))
  (face-remap-add-relative 'typescript-jsdoc-value '(:inherit jcs-oop-value-face))
  (face-remap-add-relative 'typescript-primitive-face '(:inherit font-lock-type-face))

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]ts")
                              'jcs-typescript-ask-source
                              t)

  ;; Normal
  (define-key typescript-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key typescript-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key typescript-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key typescript-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key typescript-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key typescript-mode-map (kbd "*") #'jcs-c-comment-pair))

(add-hook 'typescript-mode-hook 'jcs-typescript-mode-hook)

(provide 'jcs-typescript-mode)
;;; jcs-typescript-mode.el ends here
