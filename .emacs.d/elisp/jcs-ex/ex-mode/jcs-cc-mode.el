;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-cc-mode.el             -*- Emacs-Lisp -*-

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
;; JenChieh C/C++ mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; start auto-complete with emacs
(require 'auto-complete)

;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;; define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun jcs-ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; here we adjust the c library we want to use,
  ;; current i am using MinGW because is cross os.
  (add-to-list 'achead:include-directories '"C:/MinGW/include")
  )
;; now lets' call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'jcs-ac-c-header-init)
(add-hook 'c-mode-hook 'jcs-ac-c-header-init)

;; Fix "iedit" bug for OSX
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; start flymake-google-cpplint-load
;; let's define a function for flymake initilization
(defun jcs-flymake-google-init()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "C:/jcs_ide_packages/jcs_win7_packages/cpplint/cpplint.exe"))
  (flymake-google-cpplint-load)
  )
(add-hook 'c-mode-hook 'jcs-flymake-google-init)
(add-hook 'c++-mode-hook 'jcs-flymake-google-init)

;; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cc-mode.el file
