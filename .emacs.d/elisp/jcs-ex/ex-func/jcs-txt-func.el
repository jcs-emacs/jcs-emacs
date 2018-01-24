;; This is the start of jcs-txt-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-txt-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Wed Jan 24 12:14:56 EST 2018>
;; Time-stamp: <2018-01-24 12:14:56>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright © 2018 Jen-Chieh Shen

;; jcs-txt-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-txt-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Text mode functionalities.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar jcs-org-font-lock-comment-face-modes '(org-mode)
  "Revised version of `org-mode' comment face regexp.")

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\(#[a-z-A-Z0-9~!@#$%^&*()_+=|\'/\\]*\\)" 1 'font-lock-comment-face)
           )'end))
      jcs-org-font-lock-comment-face-modes)


;; `' electriic key.
(add-to-list 'electric-pair-pairs '(?\` . ?\'))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-txt-func.el file
