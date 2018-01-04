;; This is the start of jcs-preproc-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-preproc-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Tue Dec 14 13:51:49 EST 2017>
;; Time-stamp: <2017-12-14 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-preproc-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-preproc-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for Preprocessor Languages.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Code:


(defvar jcs-preproc-highlight-modes '(cc-mode
                                      c-mode
                                      c++-mode
                                      csharp-mode
                                      nasm-mode)
  "Modes to add preprocessor highlighting.")


(defface jcs-preproc-variable-name-face
  '((t (:foreground "#B363BE")))
  "Highlight OOP tag.")
(defvar jcs-preproc-variable-name-face 'jcs-preproc-variable-name-face)


(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("[#%]define[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%]ifdef[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%]ifndef[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%]elif[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%]if defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           ("[#%]if !defined[a-z0-9_$]*[ \t]*\\([a-zA-Z0-9_$]*\\)" 1 'jcs-preproc-variable-name-face t)
           )'end))
      jcs-preproc-highlight-modes)


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-preproc-func.el file
