;; This is the start of jcs-elisp-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-elisp-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-elisp-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-elisp-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Emacs Lisp mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-emacs-lisp-mode-hook ()
  "JayCeS Emacs Lisp hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Emacs Lisp here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-emacs-lisp-format ()
    "Format the given file as a Emacs Lisp file. - JenChieh emacs lisp file."
    (when (is-current-file-empty-p)
      (jcs-insert-elisp-template)))

  ;; NOTE(jenchieh): while loading this will get loading emacs
  ;; error, so simple add the `ignore-errors' function can avoid
  ;; this. Furthermore this will stil work after the first load.
  (ignore-errors
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]el" buffer-file-name) (jcs-emacs-lisp-format))
          )
    )

  ;; Set Faces.
  (jcs-oop-init-set-face)
  )
(add-hook 'emacs-lisp-mode-hook 'jcs-emacs-lisp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.el?\\'" . emacs-lisp-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-elisp-mode.el file
