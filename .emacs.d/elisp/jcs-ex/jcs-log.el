;; ========================================================================
;; $File: jcs-log.el $
;; $Date: 2018-01-04 17:16:25 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;; This is the start of jcs-util.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-util.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Thu Jan 04 17:16:25 EST 2018>
;; Time-stamp: <2018-01-04 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2018 Jen-Chieh Shen

;; jcs-util is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-util is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Some Debug util.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-log (format &rest args)
  "Log a log message.
FORMAT : output format.
ARGS : arguments."
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n")
  (jcs-message "$ Log : ")
  (ignore-errors
    (let ((message-log-max nil))
      (apply 'message format args))
    (with-current-buffer (get-buffer "*Messages*")
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (unless (zerop (current-column)))
          (insert (apply 'format format args))))))
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n"))

(defun jcs-error (format &rest args)
  "Log a error message.
FORMAT : output format.
ARGS : arguments."
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n")
  (jcs-message "$ Error : ")
  (ignore-errors
    (let ((message-log-max nil))
      (apply 'message format args))
    (with-current-buffer (get-buffer "*Messages*")
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (unless (zerop (current-column)))
          (insert (apply 'format format args))))))
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n"))

(defun jcs-warning (format &rest args)
  "Log a warning message.
FORMAT : output format.
ARGS : arguments."
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n")
  (jcs-message "$ Warning : ")
  (ignore-errors
    (let ((message-log-max nil))
      (apply 'message format args))
    (with-current-buffer (get-buffer "*Messages*")
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (unless (zerop (current-column)))
          (insert (apply 'format format args))))))
  (jcs-message "\n$=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=$\n"))

;;
;; TOPIC(jenchieh): How to preserve color in *Messages* buffer?
;; SOURCE(jenchieh): https://emacs.stackexchange.com/questions/20171/how-to-preserve-color-in-messages-buffer
;;
(defun jcs-message (format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
FORMAT : output format.
ARGS : arguments."
  (let ((message-log-max nil))
    (apply 'message format args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (zerop (current-column))
          ;; NOTE(jenchieh): no line break for this implementation.
          ;;(insert "\n")
          )
        (insert (apply 'format format args))
        ;; NOTE(jenchieh): no line break for this implementation.
        ;;(insert "\n")
        ))))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-util.el file
