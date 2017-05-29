;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-xml-mode.el             -*- Emacs-Lisp -*-

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
;; JenChieh XML mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-xml-mode-hook ()

  (interactive)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for XML here
  (electric-pair-mode 1)

  (defun jcs-xml-format ()
    "Format the given file as a XML file. - JenChieh XML file."

    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")
    (insert "\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]xml" buffer-file-name) (jcs-xml-format))
        )
  )
;; NOTE(jenchieh): they ae using nxml-mode instead of xml-mode
;; which is really weird.
(add-hook 'nxml-mode-hook 'jcs-xml-mode-hook)
(add-hook 'nxml-mode-hook 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.xml?\\'" . xml-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-xml-mode.el file
