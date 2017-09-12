;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-sass-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2017>
;; Time-stamp: <2017-10-21 10:21:39>
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
;; JenChieh SASS mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'ssass-mode)
(defun jcs-sass-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;;; `meghanada' Configuration
  (meghanada-mode t)

  (defun jcs-sass-file-format ()
    "Format the given file as a class. - JenChieh SASS class"
    (jcs-global-file-info)
    (insert "\n\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]sass" buffer-file-name) (jcs-sass-file-format))
        )

  ;; jcs SASS key binding
  (define-key ssass-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key ssass-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key ssass-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key ssass-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; Edit
  (define-key ssass-mode-map (kbd "<up>") 'jcs-previous-line)
  (define-key ssass-mode-map (kbd "<down>") 'jcs-next-line)
  )
(add-hook 'ssass-mode-hook 'jcs-sass-mode-hook)
(add-to-list 'auto-mode-alist '("\\.sass?\\'" . ssass-mode))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-sass-mode.el file
