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

(defun jcs-c++--docstr-after (search-string)
  "Local hook `docstr-after-insert-hook' for C++."
  (let ((splitted-search-string (split-string search-string " " t))
        (defined-keyword ""))
    (cond
     ((string-match-p "class" search-string)
      ;; Process class tag.
      (insert "@class ")
      (setq defined-keyword
            (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
      (unless defined-keyword
        (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
      (ignore-errors (insert defined-keyword))
      (indent-for-tab-command)

      ;; Process brief tag.
      (insert "\n* @brief ")
      (insert jcs--class-desc-string)
      (indent-for-tab-command))
     ((string-match-p "struct" search-string)
      ;; Process class tag.
      (insert "@struct ")
      (setq defined-keyword
            (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
      (unless defined-keyword
        (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
      (ignore-errors (insert defined-keyword))
      (indent-for-tab-command)

      ;; Process brief tag.
      (insert "\n* @brief ")
      (insert jcs--struct-desc-string)
      (indent-for-tab-command))
     ((string-match-p "#define" search-string)
      ;; Process define tag.
      (insert "@def ")
      (setq defined-keyword (nth 1 (split-string search-string " " t)))
      (ignore-errors (insert defined-keyword))
      (indent-for-tab-command)

      ;; Process brief tag.
      (insert "\n* @brief ")
      (insert jcs--define-desc-string)
      (indent-for-tab-command))
     ((string-match-p "enum" search-string)
      ;; Process enumerator tag.
      (insert "@enum ")
      (setq defined-keyword
            (jcs-find-item-in-list-offset splitted-search-string "[:{]" -1))
      (unless defined-keyword
        (setq defined-keyword (jcs-last-item-in-list splitted-search-string)))
      (ignore-errors (insert defined-keyword))
      (indent-for-tab-command)

      ;; Process brief tag.
      (insert "\n* @brief ")
      (insert jcs--enum-desc-string)
      (indent-for-tab-command)))))

;;
;; (@* "Faces" )
;;

(defconst jcs-preproc-modes
  '(cc-mode c-mode c++-mode csharp-mode objc-mode masm-mode nasm-mode)
  "List of preprocessor `major-mode'.")

(defun jcs-init-preproc-faces ()
  "Initialize preprocessor mode faces highlihgting."
  (let ((case-fold-search t))
    (dolist (mode jcs-preproc-modes)
      (font-lock-add-keywords
       mode
       '(("#pragma[ \t]+\\(comment\\)" 1 'jcs-preproc-comment-face t)
         ("#pragma[ \t]+comment[ \t]*([ \t]*\\([a-z-A-Z]+\\)[,)]" 1 'jcs-preproc-comment-type-face t))
       'end))))

(jcs-init-preproc-faces)

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

  (jcs-company-safe-add-backend 'company-clang)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  (modify-syntax-entry ?_ "w"))

(provide 'jcs-cc-mode)
;;; jcs-cc-mode.el ends here
