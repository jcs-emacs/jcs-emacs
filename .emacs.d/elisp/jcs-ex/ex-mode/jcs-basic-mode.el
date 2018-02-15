;; This is the start of jcs-basic-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-basic-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2018-02-15 15:38:16>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2018 Jen-Chieh Shen

;; jcs-basic-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-basic-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh BASIC mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'basic-mode)
(defun jcs-basic-mode-hook ()
  "Hook for `basic-mode'."


  (defun jcs-basic-script-format ()
    "Format the given file as a class. - JenChieh BASIC."
    (when (is-current-file-empty-p)
      (jcs-insert-basic-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]bas" buffer-file-name) (jcs-basic-script-format))
        )

  ;; BASIC key bindings
  (define-key basic-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key basic-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'basic-mode-hook 'jcs-basic-mode-hook)

(add-to-list 'auto-mode-alist '("\\.bas\\'" . basic-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-basic-mode.el file
