;;; jcs-typescript-mode.el --- TypeScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'typescript-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-typescript-mode-hook ()
  "TypeScript mode hook."

  (add-hook 'docstr-before-insert-hook 'jcs-typescript--docstr-before nil t)

  (face-remap-add-relative 'typescript-jsdoc-tag '(:inherit docstr-faces-tag-face))
  (face-remap-add-relative 'typescript-jsdoc-type '(:inherit docstr-faces-type-face))
  (face-remap-add-relative 'typescript-jsdoc-value '(:inherit docstr-faces-value-face))
  (face-remap-add-relative 'typescript-primitive-face '(:inherit font-lock-type-face))

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]ts")
                              'jcs-typescript-ask-source
                              :interactive t)

  ;; Normal
  (define-key typescript-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key typescript-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key typescript-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key typescript-mode-map (kbd "RET") #'jcs-smart-context-line-break))

(add-hook 'typescript-mode-hook 'jcs-typescript-mode-hook)

(provide 'jcs-typescript-mode)
;;; jcs-typescript-mode.el ends here
