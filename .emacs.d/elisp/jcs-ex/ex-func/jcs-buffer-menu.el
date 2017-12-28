;; This is the start of jcs-buffer-menu.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-buffer-menu.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Dec 29 13:51:49 EST 2017>
;; Time-stamp: <2017-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-buffer-menu is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-buffer-menu is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Function in buffer menu mode. (*Buffer List*)
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Code:

(defun jcs-buffer-menu ()
  "Open Buffer Menu."
  (interactive)

  ;; NOTE(jenchieh): even you reopen the `buffer-menu'
  ;; this will not sort.
  (buffer-menu)

  ;; Sort it?
  ;;(jcs-buffer-menu-sort)
  )

;; TOPIC(jenchieh): BufferMenuPlus
;; URL(jenchieh): https://www.emacswiki.org/emacs/BufferMenuPlus
;; Sorted by (1) visit, (2) buffer, (3) size, (4) time, (5) mode, (6) file. More
(defun jcs-buffer-menu-sort-by-visit ()
  "Sort the Buffer Menu List by visit."
  (interactive)
  (Buffer-menu-sort 1)
  (goto-char (point-min)))

(defun jcs-buffer-menu-sort-by-buffer ()
  "Sort the Buffer Menu List by buffer."
  (interactive)
  (Buffer-menu-sort 2)
  (goto-char (point-min)))

(defun jcs-buffer-menu-sort-by-size ()
  "Sort the Buffer Menu List by size."
  (interactive)
  (Buffer-menu-sort 3)
  (goto-char (point-min)))

(defun jcs-buffer-menu-sort-by-time ()
  "Sort the Buffer Menu List by time."
  (interactive)
  (Buffer-menu-sort 4)
  (goto-char (point-min)))

(defun jcs-buffer-menu-sort-by-mode ()
  "Sort the Buffer Menu List by mode."
  (interactive)
  (Buffer-menu-sort 5)
  (goto-char (point-min)))

(defun jcs-buffer-menu-sort-by-file ()
  "Sort the Buffer Menu List by file name."
  (interactive)
  (Buffer-menu-sort 6)
  (goto-char (point-min)))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-buffer-menu.el file
