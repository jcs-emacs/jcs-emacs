;;; jcs-typescript-mode.el --- TypeScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'typescript-mode)

;; Ask the source SC for editing TypeScript file.
(file-header-defsrc jcs-typescript-ask-source "Major source for this TypeScript file: "
  '("Default" "Cocos Creator Scripting")
  (pcase index
    (0 (jcs-insert-typescript-template))
    (1 (jcs-insert-typescript-cocos-creator-template))))

;;
;; (@* "Document String" )
;;

(defun jcs-typescript--docstr-before (_search-string)
  "Local hook `docstr-before-insert-hook' for TypeScript."
  (insert "@desc "))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-typescript-template "typescript" "default.txt"
  "Header for TypeScript header file.")

(file-header-defins jcs-insert-typescript-cocos-creator-template
    "typescript" "cocos_creator.txt"
  "Header for Cocos Creator TypeScript header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'typescript-mode-hook
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

  (jcs-key-local
    `(((kbd "DEL") . jcs-electric-backspace))))

(provide 'jcs-typescript-mode)
;;; jcs-typescript-mode.el ends here
