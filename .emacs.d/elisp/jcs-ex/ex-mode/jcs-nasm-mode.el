;; This is the start of jcs-nasm-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-nasm-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2017>
;; Time-stamp: <2017-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-nasm-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-nasm-mode is distributed in the hope that it will be useful,
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

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-asm-format ()
    "Format the given file as a asm code. - JenChieh Assembly Language"
    (interactive)

    (if (is-current-file-empty-p)
        (progn
          (jcs-asm-file-format)
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]asm" buffer-file-name) (jcs-asm-format))
        ((string-match "[.]inc" buffer-file-name) (jcs-asm-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs key binding
  (define-key nasm-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key nasm-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key nasm-mode-map (kbd "<up>") 'jcs-py-indent-up)
  (define-key nasm-mode-map (kbd "<down>") 'jcs-py-indent-down)

  ;; Comment
  (define-key nasm-mode-map (kbd "RET") 'jcs-nasm-return)
  (define-key nasm-mode-map (kbd ";") 'jcs-nasm-comment)

  ;; Edit
  (define-key nasm-mode-map (kbd "SPC") 'jcs-py-space)
  (define-key nasm-mode-map (kbd "S-SPC") 'jcs-py-real-space)
  (define-key nasm-mode-map (kbd "<backspace>") 'jcs-py-backspace)
  (define-key nasm-mode-map (kbd "S-<backspace>") 'jcs-py-real-backspace)
  )
(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)
(add-to-list 'auto-mode-alist '("\\.asm?\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.inc?\\'" . nasm-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-nasm-mode.el file
