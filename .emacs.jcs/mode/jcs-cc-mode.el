;;; jcs-cc-mode.el --- C/C++ Common mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'cc-mode)


;; C++ indentation style
(defconst jcs-big-fun-cc-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         . c-lineup-arglist)
                                    (label                 . -)
                                    (access-label          . -)
                                    (substatement-open     . 0)
                                    (statement-case-intro  . +)
                                    (statement-block-intro . +)
                                    (case-label            . 0)
                                    (block-open            . 0)
                                    (inline-open           . 0)
                                    (inlambda              . 0)
                                    (topmost-intro-cont    . 0)
                                    (knr-argdecl-intro     . -)
                                    (brace-list-open       . 0)
                                    (brace-list-intro      . +)))
    ;; NOTE: no more echo.
    (c-echo-syntactic-information-p . nil))
  "Casey's Big Fun C/C++ Style")

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-c-header-format ()
  "Format the given file as a C header file."
  (jcs-insert-header-if-empty 'jcs-insert-c-header-template))

(defun jcs-c-source-format ()
  "Format the given file as a C source file."
  (jcs-insert-header-if-empty 'jcs-insert-c-source-template))

(defun jcs-c++-header-format ()
  "Format the given file as a C++ header file."
  (jcs-insert-header-if-empty 'jcs-insert-c++-header-template))

(defun jcs-c++-source-format ()
  "Format the given file as a C++ source file."
  (jcs-insert-header-if-empty 'jcs-insert-c++-source-template))

(defun jcs-objc-header-format ()
  "Format the given file as a Objective-C header file."
  (jcs-insert-header-if-empty 'jcs-insert-objc-header-template))

(defun jcs-objc-source-format ()
  "Format the given file as a Objective-C source file."
  (jcs-insert-header-if-empty 'jcs-insert-objc-source-template))

(defun jcs-cc-insert-header ()
  "Insert header for `cc-mode' related modes."
  (jcs-insert-header-if-valid '("[.]hin"
                                "[.]hpp"
                                "[.]h")
                              'jcs-c++-header-format)
  (jcs-insert-header-if-valid '("[.]cin"
                                "[.]cpp")
                              'jcs-c++-source-format)
  (jcs-insert-header-if-valid '("[.]c")
                              'jcs-c-source-format))

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-cc-mode-hook ()
  "C/C++ mode hook."

  ;; Set my style for the current buffer
  (c-add-style "BigFun" jcs-big-fun-cc-style t)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  (modify-syntax-entry ?_ "w"))

(add-hook 'c-mode-common-hook 'jcs-cc-mode-hook)


(provide 'jcs-cc-mode)
;;; jcs-cc-mode.el ends here
