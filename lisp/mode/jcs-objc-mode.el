;;; jcs-objc-mode.el --- Objective-C mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
    `(([f8]          . jcs-find-corresponding-file)
      ([S-f8]        . jcs-find-corresponding-file-other-window)
      ([f7]          . jcs-same-file-other-window)  ; not the corresponding file
      ((kbd "DEL")   . jcs-electric-backspace)
      ((kbd "C-k s") . jcs-toggle-c-comment-style))))

(provide 'jcs-objc-mode)
;;; jcs-objc-mode.el ends here
