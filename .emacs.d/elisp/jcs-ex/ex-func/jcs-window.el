;; This is the start of jcs-window.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-window.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-window is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-window is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;---------------------------------------------
;; JenChieh Window Split Setting for Dual Monitor
;;---------------------------------------------
(defun jcs-new-window()
  "Setup dual monitor."
  (interactive)

  ;; open a new frame
  (make-frame-command)
  )

;;---------------------------------------------
;; Create a new frame and
;;---------------------------------------------
;; @param frame: frame we just created.
;;---------------------------------------------
(defun jcs-aftermake-frame-functions-hook (frame)
  "Resetting the new frame just created."
  (interactive)

  (select-frame frame)

  ;; split the winodw after create the new window
  (split-window-horizontally)
  )
(add-hook 'after-make-frame-functions 'jcs-aftermake-frame-functions-hook)


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-window.el file
