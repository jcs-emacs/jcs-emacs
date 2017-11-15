;; This is the start of jcs-jayces-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-jayces-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-jayces-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-jayces-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh JayCeS mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; load the mode first.
(load-file "~/.emacs.d/elisp/jayces-mode.el")

(require 'jayces-mode)
(defun jcs-jayces-mode-hook ()

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-jayces-class-format ()
    "Format the given file. - JenChieh JayCeS files"

    (if (is-current-file-empty-p)
        (progn
          (jcs-global-file-info)
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]jcs" buffer-file-name) (jcs-jayces-class-format))
        ((string-match "[.]jayces" buffer-file-name) (jcs-jayces-class-format))
        )

  )
(add-hook 'jayces-mode-hook 'jcs-jayces-mode-hook)

(add-to-list 'auto-mode-alist '("\\.jcs?\\'" . jayces-mode))
(add-to-list 'auto-mode-alist '("\\.jayces?\\'" . jayces-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-jayces-mode.el file
