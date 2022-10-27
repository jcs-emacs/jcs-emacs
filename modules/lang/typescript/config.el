;;; lang/typescript/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Document String" )
;;

(defun jcs-typescript--ts-docstr-after (node data)
  "Local hook `ts-docstr-after-insert-hook' for TypeScript."
  (insert "@desc "))

;;
;; (@* "Templates" )
;;

;; Ask the source SC for editing TypeScript file.
(file-header-defsrc jcs-typescript-ask-source "Major source for this TypeScript file: "
  '("Default" "Cocos Creator Scripting")
  (pcase index
    (0 (jcs-insert-typescript-template))
    (1 (jcs-insert-typescript-cocos-creator-template))))

(file-header-defins jcs-insert-typescript-template "typescript" "default.txt"
  "Header for TypeScript header file.")

(file-header-defins jcs-insert-typescript-cocos-creator-template
    "typescript" "cocos_creator.txt"
  "Header for Cocos Creator TypeScript header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'typescript-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  (add-hook 'ts-docstr-after-insert-hook 'jcs-typescript--ts-docstr-after nil t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]ts")
                              'jcs-typescript-ask-source
                              :interactive t))
