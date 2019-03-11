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
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-c-header-template)))

(defun jcs-c-source-format ()
  "Format the given file as a C source file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-c-source-template)))

(defun jcs-c++-header-format ()
  "Format the given file as a C++ header file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-c++-header-template)))

(defun jcs-c++-source-format ()
  "Format the given file as a C++ source file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-c++-source-template)))

(defun jcs-objc-header-format ()
  "Format the given file as a Objective-C header file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-objc-header-template)))

(defun jcs-objc-source-format ()
  "Format the given file as a Objective-C source file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-objc-source-template)))

;;-----------------------------------------------------------
;;-----------------------------------------------------------


(defun jcs-cc-mode-hook ()
  "C/C++ mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Set my style for the current buffer
  (c-add-style "BigFun" jcs-big-fun-cc-style t)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")
  )
(add-hook 'c-mode-common-hook 'jcs-cc-mode-hook)

;; define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun jcs-ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; here we adjust the c library we want to use,
  ;; current i am using MinGW because is cross os.
  (add-to-list 'achead:include-directories '"C:/MinGW/include")
  )
;; now lets' call this function from c/c++ hooks
(add-hook 'c-mode-hook 'jcs-ac-c-header-init)
(add-hook 'c++-mode-hook 'jcs-ac-c-header-init)

;; start flymake-google-cpplint-load
;; let's define a function for flymake initilization
(defun jcs-flymake-google-init()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "C:/jcs_ide_packages/jcs_win7_packages/cpplint/cpplint.exe"))
  (flymake-google-cpplint-load)
  )
(add-hook 'c-mode-hook 'jcs-flymake-google-init)
(add-hook 'c++-mode-hook 'jcs-flymake-google-init)

;; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Faces
(set-face-attribute 'preproc-font-lock-preprocessor-background nil
                    :background nil)


(provide 'jcs-cc-mode)
;;; jcs-cc-mode.el ends here
