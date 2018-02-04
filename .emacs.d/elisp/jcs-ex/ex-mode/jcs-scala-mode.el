;; This is the start of jcs-scala-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-scala-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sat Feb 03 12:59:49 EST 2018>
;; Time-stamp: <2018-02-03 12:59:49>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2018 Jen-Chieh Shen

;; jcs-scala-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-scala-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Scala mode.
;; URL(jenchieh): https://www.emacswiki.org/emacs/ScalaMode
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'scala-mode)
(defun jcs-scala-mode-hook ()
  "JayCeS Scala mode."

  ;; highlight URL and clickable.
  (goto-address-mode 1)


  (defun jcs-scala-class-format ()
    "Format the given file as a class. - JenChieh Scala."
    (when (is-current-file-empty-p)
      (jcs-insert-scala-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]scala" buffer-file-name) (jcs-scala-class-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; Scala key bindings
  ;; comment block
  (define-key scala-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key scala-mode-map (kbd "*") 'jcs-c-comment-pair)
  )
(add-hook 'scala-mode-hook 'jcs-scala-mode-hook)

(add-to-list 'auto-mode-alist '("\\.scala?\\'" . scala-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-scala-mode.el file
