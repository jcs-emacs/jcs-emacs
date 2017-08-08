;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-cc-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-function is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-function is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh C/C++ mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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
(defconst jcs-big-fun-c-style
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
  "Casey's Big Fun C++ Style")


;; C/C++ mode handling
(defun jcs-big-fun-c-hook ()

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Set my style for the current buffer
  (c-add-style "BigFun" jcs-big-fun-c-style t)

  ;; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  (defun jcs-header-format ()
    "Format the given file as a header file."

    (interactive)

    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

    (insert "#ifndef __")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H__\n")
    (jcs-global-file-info)
    (insert "#define __")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H__\n\n\n\n")
    (insert "#endif /* __")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H__ */\n")
    )

  (defun jcs-source-format ()
    "Format the given file as a source file."
    (interactive)

    (jcs-global-file-info)

    ;; macro
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

    (insert "\n")
    (insert "#include \"")
    (insert BaseFileName)
    (insert ".h\"\n\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (jcs-source-format))
        ((string-match "[.]cin" buffer-file-name) (jcs-source-format))
        ((string-match "[.]h" buffer-file-name) (jcs-header-format))
        ((string-match "[.]hpp" buffer-file-name) (jcs-header-format))
        ((string-match "[.]c" buffer-file-name) (jcs-source-format))
        ((string-match "[.]cpp" buffer-file-name) (jcs-source-format)))

  (defun casey-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
        (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
          (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
      (error "Unable to find a corresponding file")))

  (defun casey-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (casey-find-corresponding-file)
    (other-window -1))

  (define-key c++-mode-map [f8] 'casey-find-corresponding-file)
  (define-key c++-mode-map [S-f8] 'casey-find-corresponding-file-other-window)

  (define-key c-mode-map [f8] 'casey-find-corresponding-file)
  (define-key c-mode-map [S-f8] 'casey-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the
  ;; corresponding file.
  (define-key c++-mode-map [f7] 'jcs-find-file-other-window)
  (define-key c-mode-map [f7] 'jcs-find-file-other-window)

  ;; Alternate bindings for F-keyless setups (ie MacOS X terminal)
  (define-key c++-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'casey-find-corresponding-file-other-window)

  (define-key c-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c-mode-map "\eC" 'casey-find-corresponding-file-other-window)

  (define-key c++-mode-map "\es" 'casey-save-buffer)
  (define-key c-mode-map "\es" 'casey-save-buffer)

  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c++-mode-map [C-tab] 'indent-region)
  (define-key c++-mode-map "  " 'indent-region)
  ;;(define-key c++-mode-map [tab] '(lambda () (interactive) (insert "    ")))

  (define-key c-mode-map "\t" 'dabbrev-expand)
  (define-key c-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c-mode-map [C-tab] 'indent-region)
  (define-key c-mode-map "  " 'indent-region)
  ;;(define-key c-mode-map [tab] '(lambda () (interactive) (insert "    ")))

  (define-key c++-mode-map "\ej" 'imenu)
  (define-key c-mode-map "\ej" 'imenu)

  (define-key c++-mode-map "\e." 'c-fill-paragraph)
  (define-key c-mode-map "\e." 'c-fill-paragraph)

  (define-key c++-mode-map "\e/" 'c-mark-function)
  (define-key c-mode-map "\e/" 'c-mark-function)

  (define-key c++-mode-map "\eq" 'jcs-other-window-prev)
  (define-key c++-mode-map "\ea" 'yank)
  (define-key c++-mode-map "\ez" 'kill-region)

  (define-key c-mode-map "\eq" 'jcs-other-window-prev)
  (define-key c-mode-map "\ea" 'yank)
  (define-key c-mode-map "\ez" 'kill-region)

  ;; jcs-added
  (define-key c++-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key c++-mode-map "\C-c\C-c" 'kill-ring-save)

  (define-key c-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key c-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; Comment Block.
  (define-key c-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key c++-mode-map (kbd "RET") 'jcs-smart-context-line-break)

  (define-key c-mode-map (kbd "*") 'jcs-c-comment-pair)
  (define-key c++-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'casey-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(casey-devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4)))

  ;; Turn on line numbers
  ;;(linum-mode)
  ;; or Turn on globale line numbers
  (global-linum-mode)
  )
(add-hook 'c-mode-common-hook 'jcs-big-fun-c-hook)

;; start auto-complete with emacs
(require 'auto-complete)

;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;; define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun jcs-ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; here we adjust the c library we want to use,
  ;; current i am using MinGW because is cross os.
  (add-to-list 'achead:include-directories '"C:/MinGW/include")
  )
;; now lets' call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'jcs-ac-c-header-init)
(add-hook 'c-mode-hook 'jcs-ac-c-header-init)

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

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cc-mode.el file
