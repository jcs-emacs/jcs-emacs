;;; lang/c/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Header" )
;;

(file-header-defsrc jcs-c-ask-header "Type of header inclusion: "
  '(("pragma once"    . "Pragma once inclusion")
    ("include gaurds" . "Include gaurds inclusion"))
  (pcase index
    (0 (jcs-insert-c-header-template-pragma-once))
    (1 (jcs-insert-c-header-template-include-guards))))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-c-header-template-pragma-once
    "c" "header/pragma_once.txt"
  "Header for C header file (pragma once).")

(file-header-defins jcs-insert-c-header-template-include-guards
    "c" "header/include_guards.txt"
  "Header for C header file (include guards).")

(file-header-defins jcs-insert-c-source-template
    "c" "source.txt"
  "Header for C source file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'c-mode-hook
  (company-fuzzy-backend-add-before 'company-c-headers 'company-dabbrev)

  ;; File Header
  (jcs-cc-insert-header)

  (jcs-key-local
    `(((kbd "C-k s") . jcs-toggle-c-comment-style))))
