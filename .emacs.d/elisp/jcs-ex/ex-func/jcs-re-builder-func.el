;; This is the start of jcs-re-builder-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-re-builder-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Mon Nov 27 13:51:49 EST 2017>
;; Time-stamp: <2017-11-27 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-re-builder-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-re-builder-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh RE-Builder functions.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-reb-maybe-kill-this-buffer ()
  "Kill this buffer in `re-builder' mode."
  (interactive)
  
  ;; maybe kill this buffer.
  (jcs-maybe-kill-this-buffer)

  ;; then delete this window.
  (delete-window)
  )


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-re-builder-func.el file
