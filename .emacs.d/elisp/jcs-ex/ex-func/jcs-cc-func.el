;; This is the start of jcs-cc-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-cc-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sun Nov 20 13:51:49 EST 2017>
;; Time-stamp: <2017-11-20 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-cc-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-cc-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for C/C++ common.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Code:

;;;###autoload
(defun jcs-toggle-cc-mode ()
  "Toggle c/c++ mode."
  (interactive)

  (if (equal major-mode 'c-mode)
      (progn
        (c++-mode))
    (progn
      (c-mode))))

(defun jcs-ask-cpp-default-header (bool)
  (interactive
   (list (y-or-n-p "Do you want to add C++ default class template? ")))

  (if bool
      (progn
        (c++-mode)
        (jcs-c++-default-header-template))
    (c-mode)))

(defun jcs-ask-cpp-default-source (bool)
  (interactive
   (list (y-or-n-p "Do you want to add C++ default class template? ")))

  (if bool
      (progn
        (c++-mode)
        (jcs-c++-default-source-template))
    (c-mode)))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cc-func.el file
