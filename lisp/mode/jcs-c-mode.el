;;; jcs-c-mode.el --- C mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jcs-cc-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-c-header-template ()
  "Header for C header file."
  (jcs--file-header--insert "c" "header.txt"))

(defun jcs-insert-c-source-template ()
  "Header for C source file."
  (jcs--file-header--insert "c" "source.txt"))

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
      ((kbd "{")     . jcs-vs-opening-curly-bracket-key)
      ((kbd ";")     . jcs-vs-semicolon-key)
      ((kbd "C-k s") . jcs-toggle-c-comment-style)
      ((kbd "#")     . jcs-vs-sharp-key)
      ((kbd "C-z")   . jcs-undo)
      ((kbd "C-y")   . jcs-redo))))

(provide 'jcs-c-mode)
;;; jcs-c-mode.el ends here
