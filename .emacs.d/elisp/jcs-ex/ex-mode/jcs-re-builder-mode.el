;; This is the start of jcs-re-builder-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-re-builder-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Mon Nov 27 13:51:49 EST 2017>
;; Time-stamp: <2017-11-27 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-re-builder-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-re-builder-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh RE-Builder mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 're-builder)

(defun jcs-re-builder-mode-hook ()
  "Mode hook for `RE-Builder-mode'."

  (define-key reb-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key reb-mode-map "\C-c\C-c" 'kill-ring-save)

  (define-key reb-mode-map "\ek" 'jcs-reb-maybe-kill-this-buffer)
  )

(add-hook 'reb-mode-hook 'jcs-re-builder-mode-hook)

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-re-builder-mode.el file
