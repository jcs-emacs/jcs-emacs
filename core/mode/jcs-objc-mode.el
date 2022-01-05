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

(defun jcs-objc-mode-hook ()
  "Objective-C mode hook."

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

  ;; Normal
  (jcs-bind-key [f8] #'jcs-find-corresponding-file)
  (jcs-bind-key [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the corresponding file.
  (jcs-bind-key [f7] #'jcs-same-file-other-window)

  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  ;; Comement
  (jcs-bind-key (kbd "C-k s") #'jcs-toggle-c-comment-style)

  ;; Undo/Redo
  (jcs-bind-key (kbd "C-z") #'jcs-undo)
  (jcs-bind-key (kbd "C-y") #'jcs-redo))

(add-hook 'objc-mode-hook 'jcs-objc-mode-hook)

(provide 'jcs-objc-mode)
;;; jcs-objc-mode.el ends here
