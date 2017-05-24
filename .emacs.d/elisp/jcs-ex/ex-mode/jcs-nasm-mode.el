;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-nasm-mode.el             -*- Emacs-Lisp -*-

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
;; JenChieh Assembly Language mode. (NASM)
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'nasm-mode)
(defun jcs-nasm-mode-hook()
  ;;
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  (defun jcs-asm-format ()
    "Format the given file as a asm code. - JenChieh Assembly Language"
    (interactive)

    (jcs-asm-file-format)

    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]asm" buffer-file-name) (jcs-asm-format))
        ((string-match "[.]inc" buffer-file-name) (jcs-asm-format))
        )

  ;; jcs key binding
  (define-key nasm-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key nasm-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key nasm-mode-map (kbd "<up>") 'previous-line)
  (define-key nasm-mode-map (kbd "<down>") 'next-line)
  )
(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)
(add-to-list 'auto-mode-alist '("\\.asm?\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.inc?\\'" . nasm-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-nasm-mode.el file
