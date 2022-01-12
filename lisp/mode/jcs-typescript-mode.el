;;; jcs-typescript-mode.el --- TypeScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'typescript-mode)

(defun jcs-typescript-ask-source (sc)
  "Ask the source SC for editing TypeScript file."
  (interactive
   (list (completing-read
          "Major source for this TypeScript file: "
          '("Default" "Cocos Creator Scripting"))))
  (pcase sc
    ("Default" (jcs-insert-typescript-template))
    ("Cocos Creator Scripting" (jcs-insert-typescript-cocos-creator-template))))

;;
;; (@* "Document String" )
;;

(defun jcs-typescript--docstr-before (_search-string)
  "Local hook `docstr-before-insert-hook' for TypeScript."
  (insert "@desc "))

;;
;; (@* "Templates" )
;;

(defun jcs-insert-typescript-template ()
  "Header for TypeScript header file."
  (jcs--file-header--insert "typescript" "default.txt"))

(defun jcs-insert-typescript-cocos-creator-template ()
  "Header for Cocos Creator TypeScript header file."
  (jcs--file-header--insert "typescript" "cocos_creator.txt"))

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
    `(((kbd "DEL") . jcs-electric-backspace)
      ((kbd "{")   . jcs-vs-opening-curly-bracket-key)
      ((kbd ";")   . jcs-vs-semicolon-key))))

(provide 'jcs-typescript-mode)
;;; jcs-typescript-mode.el ends here
