;;; lang/cc/config.el  -*- lexical-binding: t; -*-

(require 'company-c-headers)

;;
;; (@* "Settings" )
;;

(msg-clean-add-mute-commands '( company-clang--handle-error))

;;
;; (@* "Keys" )
;;

(defun jcs-toggle-c-comment-style ()
  "Toggle comment style between /* */ and //."
  (interactive)
  (when (memq major-mode '(c-mode c++-mode))
    (if (string= comment-start "// ")
        (setq comment-start "/*"
              comment-start-skip "/\\*+[[:space:]]*"
              comment-end "*/"
              comment-end-skip "[[:space:]]*\\*+/")
      (setq comment-start "// "
            comment-end ""))))

;;
;; (@* "Commenting" )
;;

(defun jcs-use-cc-mutliline-comment ()
  "Fixed multiline comment."
  (require 'typescript-mode)
  (setq-local indent-line-function 'typescript-indent-line)
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (let ((c-buffer-is-cc-mode t))
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables)))

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
  (when-let* ((name (plist-get data :name)))
    (ts-docstr-with-insert-indent
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
;; (@* "Style" )
;;

(defun jcs-cc-lineup-arglist-close (langlem)
  "Line up the closing brace in an arglist with the opening brace IF cursor is
preceded by the opening brace or a comma (disregarding whitespace in between)."
  (when (save-excursion
          (save-match-data
            (skip-chars-backward " \t\n" (c-langelem-pos langelem))
            (memq (char-before) (list ?, ?\( ?\;))))
    (c-lineup-arglist langlem)))

(c-add-style
 "jcs" '((c-comment-only-line-offset . 0)
         (c-hanging-braces-alist (brace-list-open)
                                 (brace-entry-open)
                                 (substatement-open after)
                                 (block-close . c-snug-do-while)
                                 (arglist-cont-nonempty))
         (c-cleanup-list brace-else-brace)
         (c-offsets-alist
          (knr-argdecl-intro . 0)
          (substatement-open . 0)
          (substatement-label . 0)
          (statement-cont . +)
          (case-label . +)
          ;; align args with open brace OR don't indent at all (if open
          ;; brace is at eolp and close brace is after arg with no trailing
          ;; comma)
          (brace-list-intro . 0)
          (brace-list-close . -)
          (arglist-intro . +)
          (arglist-close jcs-cc-lineup-arglist-close 0)
          ;; don't over-indent lambda blocks
          (inline-open . 0)
          (inlambda . 0)
          ;; indent access keywords +1 level, and properties beneath them
          ;; another level
          (access-label . -)
          (label . 0))))

(when (listp c-default-style)
  (setf (alist-get 'other c-default-style) "jcs"))

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
  (jcs-insert-header-if-valid jcs-c++-header-extensions 'jcs-c++-ask-header :interactive t)
  (jcs-insert-header-if-valid jcs-c++-source-extensions 'jcs-insert-c++-source-template)
  (jcs-insert-header-if-valid jcs-c-header-extensions   'jcs-c-ask-header :interactive t)
  (jcs-insert-header-if-valid jcs-c-source-extensions   'jcs-insert-c-source-template))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'c-mode-common-hook
  (company-fuzzy-backend-add-before 'company-clang 'company-dabbrev)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-clang-analyzer
  :hook (flycheck-mode . flycheck-clang-analyzer-setup))
