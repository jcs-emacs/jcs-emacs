;; This is the start of jcs-oop-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-oop-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Tue Nov 21 13:51:49 EST 2017>
;; Time-stamp: <2017-11-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-oop-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-oop-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for Object Oriented Programming languages.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar jcs-oop-highlight-modes '(actionscript-mode
                                  cc-mode
                                  c-mode
                                  c++-mode
                                  csharp-mode
                                  ;;jdee-mode  ;; Java has their own doc highlighting.
                                  jayces-mode
                                  js2-mode
                                  lua-mode
                                  nasm-mode
                                  php-mode
                                  web-mode))

(defface jcs-oop-tag-face
  '((t (:foreground "#38EFCA")))
  "Highlight OOP tag.")
(defvar jcs-oop-tag-face 'jcs-oop-tag-face)

(defface jcs-oop-type-face
  '((t (:foreground "SteelBlue")))
  "Highlight OOP type.")
(defvar jcs-oop-type-face 'jcs-oop-type-face)

(defface jcs-oop-value-face
  '((t (:foreground "PeachPuff3")))
  "Highlight OOP value.")
(defvar jcs-oop-value-face 'jcs-oop-value-face)


;; STUDY(jenchieh): https://stackoverflow.com/questions/5073930/how-to-color-at-symbol-in-emacs
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\(?:^\\|\\s-\\)\\(@[a-zA-Z0-9_]*\\)" 1 'jcs-oop-tag-face t)
           ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
           ;; OPTION(jenchieh): Highlight curly bracket.
           ("[`*].*\\(?:^\\|\\s-\\)\\({.*.}\\)" 1 'jcs-oop-type-face t)
           ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           ;; OR(jenchieh):
           ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           ;; OPTION(jenchieh): Don't highlight curly bracket.
           ;;("[`*].*\\(?:^\\|\\s-\\){\\(.*.\\)}" 1 'jcs-oop-type-face t)
           ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
           ("[`*].*}.\\([a-zA-Z0-9_]*\\).[-\|:]" 1 'jcs-oop-value-face t)
           )))
      jcs-oop-highlight-modes)

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-oop-func.el file
