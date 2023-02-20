;;; lang/objc/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-objc-header-template "objc" "header.txt"
  "Header for Objective-C header file.")

(file-header-defins jcs-insert-objc-source-template "objc" "source.txt"
  "Header for Objective-C source file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'objc-mode-hook
  (company-fuzzy-backend-add 'company-c-headers)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]hin"
                                "[.]hpp"
                                "[.]h")
                              'jcs-insert-objc-header-template)
  (jcs-insert-header-if-valid '("[.]cin"
                                "[.]cpp"
                                "[.]c"
                                "[.]m")
                              'jcs-insert-objc-source-template)

  (jcs-key-local
    `(((kbd "C-k s") . jcs-toggle-c-comment-style))))
