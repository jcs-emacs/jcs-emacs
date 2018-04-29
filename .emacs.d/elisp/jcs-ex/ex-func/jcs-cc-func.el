;; This is the start of jcs-cc-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-cc-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sun Nov 20 13:51:49 EST 2017>
;; Time-stamp: <2017-11-20 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-cc-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-cc-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for C/C++ common.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Code:

;;;###autoload
(defun jcs-toggle-cc-mode ()
  "Toggle c/c++ mode."
  (interactive)
  (cond ((equal major-mode 'c-mode) (progn (c++-mode)))
        ((equal major-mode 'c++-mode) (progn (c-mode)))))

;;;###autoload
(defun jcs-toggle-c-comment-style ()
  "Toggle comment style between /* */ and //."
  (interactive)

  (when (or (jcs-is-current-major-mode-p "c-mode")
            (jcs-is-current-major-mode-p "c++-mode"))
    (if (string= comment-start "// ")
        (progn
          (setq comment-start "/*"
                comment-start-skip "/\\*+[ \t]*"
                comment-end "*/"
                comment-end-skip "[ \t]*\\*+/"))
      (progn
        (setq comment-start "// "
              comment-end "")))))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cc-func.el file
