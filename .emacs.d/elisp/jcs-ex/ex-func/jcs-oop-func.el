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


(setq oop-highlight-modes '(actionscript-mode
                            cc-mode
                            c-mode
                            c++-mode
                            csharp-mode
                            ;;jdee-mode  ;; Java has their own doc highlighting.
                            js2-mode
                            lua-mode
                            php-mode
                            web-mode))

(make-face 'font-lock-oop-face)

;; STUDY(jenchieh): https://stackoverflow.com/questions/5073930/how-to-color-at-symbol-in-emacs
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\(?:^\\|\\s-\\)\\(@[a-zA-Z0-9_]*\\)" 1 'font-lock-oop-face t)
           )))
      oop-highlight-modes)

(modify-face 'font-lock-oop-face "#38EFCA" nil nil nil nil nil nil nil)

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-oop-func.el file
