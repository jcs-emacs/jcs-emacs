;; This is the start of jcs-c-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-c-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-c-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-c-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh C mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defun jcs-c-mode-hook ()
  "C mode handling"

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (jcs-c++-header-format))
        ((string-match "[.]hpp" buffer-file-name) (jcs-c++-header-format))
        ((string-match "[.]h" buffer-file-name) (jcs-c++-header-format))

        ((string-match "[.]cin" buffer-file-name) (jcs-c++-source-format))
        ((string-match "[.]cpp" buffer-file-name) (jcs-c++-source-format))
        ((string-match "[.]c" buffer-file-name) (jcs-c-source-format))
        )

  (defun casey-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (let ((CorrespondingFileName nil)
          (BaseFileName (file-name-sans-extension buffer-file-name)))
      (if (string-match "\\.c" buffer-file-name)
          (setq CorrespondingFileName (concat BaseFileName ".h")))
      (if (string-match "\\.h" buffer-file-name)
          (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
            (setq CorrespondingFileName (concat BaseFileName ".c"))))
      (if (string-match "\\.hin" buffer-file-name)
          (setq CorrespondingFileName (concat BaseFileName ".cin")))
      (if (string-match "\\.cin" buffer-file-name)
          (setq CorrespondingFileName (concat BaseFileName ".hin")))
      (if (string-match "\\.cpp" buffer-file-name)
          (setq CorrespondingFileName (concat BaseFileName ".h")))
      (if CorrespondingFileName (find-file CorrespondingFileName)
        (error "Unable to find a corresponding file"))))

  (defun casey-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (casey-find-corresponding-file)
    (other-window -1))


  ;; Set Faces.
  ;; URL(jenchieh): http://ergoemacs.org/emacs/elisp_define_face.html
  (setq-local font-lock-comment-face '(jdee-font-lock-javadoc-face))

  ;; jcs C key binding
  (define-key c-mode-map [f8] 'casey-find-corresponding-file)
  (define-key c-mode-map [S-f8] 'casey-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the
  ;; corresponding file.
  (define-key c-mode-map [f7] 'jcs-find-file-other-window)

  ;; Alternate bindings for F-keyless setups (ie MacOS X terminal)
  (define-key c-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c-mode-map "\eC" 'casey-find-corresponding-file-other-window)

  (define-key c-mode-map "\es" 'casey-save-buffer)

  (define-key c-mode-map "\t" 'dabbrev-expand)
  (define-key c-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c-mode-map [C-tab] 'indent-region)
  (define-key c-mode-map "  " 'indent-region)
  ;;(define-key c-mode-map [tab] '(lambda () (interactive) (insert "    ")))

  (define-key c-mode-map "\ej" 'imenu)

  (define-key c-mode-map "\e." 'c-fill-paragraph)

  (define-key c-mode-map "\e/" 'c-mark-function)

  (define-key c-mode-map "\eq" 'jcs-other-window-prev)
  (define-key c-mode-map "\ea" 'yank)
  (define-key c-mode-map "\ez" 'kill-region)

  ;; jcs-added
  (define-key c-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key c-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; Comment Block.
  (define-key c-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key c-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'casey-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(casey-devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4)))
  )
(add-hook 'c-mode-hook 'jcs-c-mode-hook)

;;(add-to-list 'auto-mode-alist '("\\.h?\\'" . c-mode))
;;(add-to-list 'auto-mode-alist '("\\.c?\\'" . c-mode))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-c-mode.el file
