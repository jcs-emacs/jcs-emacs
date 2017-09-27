;; This is the start of jcs-util.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-util.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-minimap is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-minimap is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Minimap.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;;;###autoload
(defun jcs-toggle-minimap ()
  "Toggle minimap. (sublimity)"
  (interactive)

  (if (get 'jcs-toggle-minimap 'state)
      (progn
        (setq sublimity-map-size 0)
        ;; ATTENTION(jenchieh): Set it to very hight so it
        ;; will never reach the timer error.
        (sublimity-map-set-delay 40000000)
        (put 'jcs-toggle-minimap 'state nil))
    (progn
      (setq sublimity-map-size 10)
      ;; NOTE: Set it to nil cost too many performanc...
      (sublimity-map-set-delay 0)
      (put 'jcs-toggle-minimap 'state t)))
  )



;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-util.el file
