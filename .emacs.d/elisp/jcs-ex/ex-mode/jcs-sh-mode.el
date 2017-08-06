;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-sh-mode.el             -*- Emacs-Lisp -*-

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
;; JenChieh Shell mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'sh-script)
(defun jcs-sh-script-hook()

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (defun jcs-sh-script-format ()

    "Format the given file as a shell script. - JenChieh Shell Script"

    (jcs-manage-file-info)
    )


  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]sh" buffer-file-name) (jcs-sh-script-format))
        )

  ;; jcs key binding
  (define-key sh-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key sh-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'sh-mode-hook 'jcs-sh-script-hook)

(add-to-list 'auto-mode-alist '("\\.sh?\\'" . sh-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-sh-mode.el file
