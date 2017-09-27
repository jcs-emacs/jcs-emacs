;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-scss-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2017>
;; Time-stamp: <2017-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-scss-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-scss-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh SCSS mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'scss-mode)
(defun jcs-scss-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;;; `meghanada' Configuration
  (meghanada-mode t)

  (defun jcs-scss-file-format ()
    "Format the given file as a class. - JenChieh SCSS class"
    (jcs-global-file-info)
    (insert "\n\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]scss" buffer-file-name) (jcs-scss-file-format))
        )

  ;; jcs SCSS key binding
  (define-key scss-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key scss-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key scss-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key scss-mode-map (kbd "*") 'jcs-c-comment-pair)
  )
(add-hook 'scss-mode-hook 'jcs-scss-mode-hook)
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . scss-mode))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-scss-mode.el file
