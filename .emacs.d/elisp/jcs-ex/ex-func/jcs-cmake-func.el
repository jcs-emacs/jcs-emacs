;; This is the start of jcs-cmake-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-cmake-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sun Dec 04 13:51:49 EST 2017>
;; Time-stamp: <2017-12-04 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-cmake-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-cmake-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for CMake and Makefile.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Code:


(defun jcs-ask-makefile-app-template (bool)
  (interactive
   (list (y-or-n-p "Add makefile application template?  ")))

  (if bool
      (progn
        ;; Insert app template.
        (jcs-makefile-app-template))
    (progn
      ;; Else we ask to add lib template.
      (call-interactively 'jcs-ask-makefile-lib-template))
    ))

(defun jcs-ask-makefile-lib-template (bool)
  (interactive
   (list (y-or-n-p "Add makefile library template?  ")))

  (if bool
      (progn
        ;; Insert lib template.
        (jcs-makefile-lib-template)
        )
    (progn
      ;; TODO(jenchieh): Else we ask to add another type?
      ))
  )

(defun jcs-makefile-newline ()
  "Newline"
  (interactive)
  (insert "\n")
  (py-indent-line-outmost))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cmake-func.el file
