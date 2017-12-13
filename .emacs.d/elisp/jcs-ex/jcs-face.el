;; This is the start of jcs-face.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-face.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Wed Dec 13 13:51:49 EST 2017>
;; Time-stamp: <2017-12-13 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-face is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-face is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(defface jcs-font-lock-comment-face
  '((t (:foreground "olive drab")))
  "JCS local comment face."
  :group 'basic-faces)
(defvar jcs-font-lock-comment-face 'jcs-font-lock-comment-face)

(defface jcs-font-lock-string-face
  '((t (:foreground "#D69D78")))
  "JCS local string face."
  :group 'basic-faces)
(defvar jcs-font-lock-string-face 'jcs-font-lock-string-face)

(defface jcs-font-lock-preprocessor-face
  '((t (:foreground "#8D9B99")))
  "JCS local preprocessor face."
  :group 'basic-faces)
(defvar jcs-font-lock-preprocessor-face 'jcs-font-lock-preprocessor-face)


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-face.el file
