;;; jcs-cc-mode.el --- C/C++ Common mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.h$"    .c++-mode)
         ("\\.hpp$"    .c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ) auto-mode-alist))

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
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    ;; NOTE(jenchieh): no more echo.
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

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-cc-mode-hook ()
  "C/C++ mode hook."
  (abbrev-mode 1)
  (auto-highlight-symbol-mode t)
  (goto-address-mode 1)

  ;; Set my style for the current buffer
  (c-add-style "BigFun" jcs-big-fun-cc-style t)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; TOPIC: Treat underscore as word.
  ;; URL: https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")
  )
(add-hook 'c-mode-common-hook 'jcs-cc-mode-hook)


(provide 'jcs-cc-mode)
;;; jcs-cc-mode.el ends here
