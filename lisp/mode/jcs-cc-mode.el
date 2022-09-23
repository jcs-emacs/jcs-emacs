;;; jcs-cc-mode.el --- C like programming language modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cc-mode)
(require 'company-c-headers)

(defun jcs-toggle-c-comment-style ()
  "Toggle comment style between /* */ and //."
  (interactive)
  (when (memq major-mode '(c-mode c++-mode))
    (if (string= comment-start "// ")
        (setq comment-start "/*"
              comment-start-skip "/\\*+[ \t]*"
              comment-end "*/"
              comment-end-skip "[ \t]*\\*+/")
      (setq comment-start "// "
            comment-end ""))))

;;
;; (@* "Document String" )
;;

(defvar jcs--class-desc-string "Class description here.."
  "Class description string.")
(defvar jcs--struct-desc-string "Struct description here.."
  "Struct description string.")
(defvar jcs--define-desc-string "Define description here.."
  "Define description string.")
(defvar jcs--enum-desc-string "Enum description here.."
  "Enum description string.")

(defun jcs-c++--ts-docstr-after (node data)
  "Local hook `ts-docstr-after-insert-hook' for C++."
  (when-let ((name (plist-get data :name)))
    (ts-docstr-inserting
      (cl-case (tsc-node-type node)
        (preproc_def
         (insert "@def " name "\n")
         (insert "* @brief " jcs--define-desc-string))
        (class_specifier
         (insert "@class " name "\n")
         (insert "* @brief " jcs--class-desc-string))
        (struct_specifier
         (insert "@struct " name "\n")
         (insert "* @brief " jcs--struct-desc-string))
        (enum_specifier
         (insert "@enum " name "\n")
         (insert "* @brief " jcs--enum-desc-string)))
      (setq restore-point (point)))))

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

(jcs-add-hook 'c-mode-common-hook
  (jcs-use-cc-mutliline-comment)

  (company-fuzzy-backend-add 'company-clang)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  (modify-syntax-entry ?_ "w"))

(provide 'jcs-cc-mode)
;;; jcs-cc-mode.el ends here
