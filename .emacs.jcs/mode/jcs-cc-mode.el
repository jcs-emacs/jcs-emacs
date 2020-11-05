;;; jcs-cc-mode.el --- C/C++ Common mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cc-mode)

(require 'company-c-headers)

;;
;; (@* "Header" )
;;

(defconst jcs-c-header-extensions '("[.]h")
  "List of C header file extension.")

(defconst jcs-c-source-extensions '("[.]c")
  "List of C source file extension.")

(defconst jcs-c++-header-extensions '("[.]hin" "[.]hpp")
  "List of C++ header file extension.")

(defconst jcs-c++-source-extensions '("[.]cin" "[.]cpp")
  "List of C++ source file extension.")

(defun jcs-cc-insert-header ()
  "Insert header for `cc-mode' related modes."
  (jcs-insert-header-if-valid jcs-c++-header-extensions 'jcs-insert-c++-header-template)
  (jcs-insert-header-if-valid jcs-c++-source-extensions 'jcs-insert-c++-source-template)
  (jcs-insert-header-if-valid jcs-c-header-extensions 'jcs-insert-c-header-template)
  (jcs-insert-header-if-valid jcs-c-source-extensions 'jcs-insert-c-source-template))

;;
;; (@* "Hook" )
;;

(defun jcs-cc-mode-hook ()
  "C/C++ mode hook."
  (jcs-use-cc-mutliline-comment)

  (jcs-company-safe-add-backend 'company-clang)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  (modify-syntax-entry ?_ "w"))

(add-hook 'c-mode-common-hook 'jcs-cc-mode-hook)

(provide 'jcs-cc-mode)
;;; jcs-cc-mode.el ends here
