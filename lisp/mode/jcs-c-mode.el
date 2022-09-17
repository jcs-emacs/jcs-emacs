;;; jcs-c-mode.el --- C mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jcs-cc-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-c-header-template "c" "header.txt"
  "Header for C header file.")

(file-header-defins jcs-insert-c-source-template "c" "source.txt"
  "Header for C source file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'c-mode-hook
  (company-fuzzy-backend-add 'company-c-headers)

  ;; File Header
  (jcs-cc-insert-header)

  (jcs-key-local
    `(([f8]          . jcs-find-corresponding-file)
      ([S-f8]        . jcs-find-corresponding-file-other-window)
      ([f7]          . jcs-same-file-other-window)  ; not the corresponding file .
      ((kbd "DEL")   . jcs-electric-backspace)
      ((kbd "C-k s") . jcs-toggle-c-comment-style))))

(provide 'jcs-c-mode)
;;; jcs-c-mode.el ends here
