;; This is the start of jcs-after-init.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-after-init.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Thu Aug 04 13:51:49 EST 2017>
;; Time-stamp: <2017-08-04 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-after-init is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-after-init is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Do stuff after initialize.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; NOTE(JenChieh): .cs add-to-list action above will
;;                  override the .c file c-mode add-to-list
;;                  action, so i have to override it agian
;;                  so it won't cover the setting i want...
(add-to-list 'auto-mode-alist '("\\.c?\\'" . c-mode))

;;; Override all the mode's key bindings.
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-global-key.el")

;;;###autoload
(defun jcs-focus-in-hook ()
  "When window is focus."
  (interactive)
  )

;;;###autoload
(defun jcs-focus-out-hook ()
  "When window is not focus."
  (interactive)
  )

(add-hook 'focus-in-hook 'jcs-focus-in-hook)
(add-hook 'focus-out-hook 'jcs-focus-out-hook)

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-after-init.el file
