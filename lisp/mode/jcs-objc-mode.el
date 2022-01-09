;;; jcs-objc-mode.el --- Objective-C mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Templates" )
;;

(defun jcs-insert-objc-header-template ()
  "Header for Objective-C header file."
  (jcs--file-header--insert "objc" "header.txt"))

(defun jcs-insert-objc-source-template ()
  "Header for Objective-C source file."
  (jcs--file-header--insert "objc" "source.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'objc-mode-hook
  (jcs-company-safe-add-backend 'company-c-headers)

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
    `(([f8]   . jcs-find-corresponding-file)
      ([S-f8] . jcs-find-corresponding-file-other-window)

      ;; If just want to open the same file, not the corresponding file.
      ([f7] . jcs-same-file-other-window)

      ((kbd "DEL") . jcs-electric-backspace)
      ((kbd "{") . jcs-vs-opening-curly-bracket-key)
      ((kbd ";") . jcs-vs-semicolon-key)

      ;; Comement
      ((kbd "C-k s") . jcs-toggle-c-comment-style)

      ;; Undo/Redo
      ((kbd "C-z") . jcs-undo)
      ((kbd "C-y") . jcs-redo))))

(provide 'jcs-objc-mode)
;;; jcs-objc-mode.el ends here
