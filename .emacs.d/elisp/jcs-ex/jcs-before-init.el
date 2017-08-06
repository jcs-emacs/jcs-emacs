;; This is the start of jcs-before-init.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-before-init.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sat Jun 30 13:51:49 EST 2017>
;; Time-stamp: <2017-07-30 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-file-info-format is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-file-info-format is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;================================================
;; Reset all the settings before starting initialize
;; the jcs-package.
;;================================================

;;; Needed packages.

;;; unbind the key
(global-unset-key "\C-k")
(global-unset-key "\C-f")
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-w"))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-before-init.el file
