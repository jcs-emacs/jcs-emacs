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


(defun jcs-ask-makefile-language (lan)
  "Ask makefile what major language is this makefile going to use.
Then specialize makefile to that target language.

LAN : temporary variable store user langauge input."

  (interactive
   (list (completing-read
          "Major language for this Makfile: " '("Default (empty)"
                                                "Assembly"
                                                "C"
                                                "C++"
                                                "Java"
                                                "Python"))))

  (cond ((string= lan "Default (empty)")
         (progn
           ;; Empty..
           ))
        ((string= lan "Assembly")
         (progn
           (call-interactively 'jcs-ask-makefile-cc-template)
           ))
        ((string= lan "C")
         (progn
           (call-interactively 'jcs-ask-makefile-cc-template)
           ))
        ((string= lan "C++")
         (progn
           (call-interactively 'jcs-ask-makefile-cc-template)
           ))
        ((string= lan "Java")
         (progn
           (call-interactively 'jcs-ask-makefile-java-template)
           ))
        ((string= lan "Python")
         (progn
           (call-interactively 'jcs-ask-makefile-python-template)
           ))
        )
  )

;;;###autoload
(defun jcs-ask-makefile-cc-template (type)
  "Ask makefile template type in Assembly, C, C++.
TYPE: type of makefile for Assembly and C/C++."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))

  (cond ((string= type "..")
         (progn
           (call-interactively 'jcs-ask-makefile-language)
           ))
        ((string= type "Application")
         (progn
           (jcs-insert-makefile-cc-app-template)))
        ((string= type "Library")
         (progn
           (jcs-insert-makefile-cc-lib-template)))
        ))


;;;###autoload
(defun jcs-ask-makefile-java-template (type)
  "Ask makefile template type in Java.
TYPE: type of makefile for Java."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))

  (cond ((string= type "..")
         (progn
           (call-interactively 'jcs-ask-makefile-language)
           ))
        ((string= type "Application")
         (progn
           (jcs-insert-makefile-java-app-template)))
        ((string= type "Library")
         (progn
           (jcs-insert-makefile-java-lib-template)))
        ))


;;;###autoload
(defun jcs-ask-makefile-python-template (type)
  "Ask makefile template type in Python.
TYPE: type of makefile for Python."
  (interactive
   (list (completing-read
          "Type of makefile: " '(".."
                                 "Application"
                                 "Library"))))

  (cond ((string= type "..")
         (progn
           (call-interactively 'jcs-ask-makefile-language)
           ))
        ((string= type "Application")
         (progn
           (jcs-insert-makefile-python-app-template)))
        ((string= type "Library")
         (progn
           (jcs-insert-makefile-python-lib-template)))
        ))



;;;###autoload
(defun jcs-makefile-newline ()
  "Newline"
  (interactive)
  (insert "\n")
  (py-indent-line-outmost))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cmake-func.el file
