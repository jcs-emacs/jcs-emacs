;; This is the start of jcs-perl-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-perl-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing Perl code

;; Created:    <Sun Feb 04 22:02:36 EST 2018>
;; Time-stamp: <2018-02-04 22:02:36>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2018 Jen-Chieh Shen

;; jcs-perl-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-perl-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Perl mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'perl-mode)
(defun jcs-perl-mode-hook ()
  "JayCeS Perl mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-perl-script-format ()
    "Format the given file as a shell script. - JenChieh Shell Script"
    (when (is-current-file-empty-p)
      (jcs-insert-perl-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]pl" buffer-file-name) (jcs-perl-script-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs key binding
  (define-key perl-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key perl-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'perl-mode-hook 'jcs-perl-mode-hook)

(add-to-list 'auto-mode-alist '("\\.pl?\\'" . perl-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-perl-mode.el file
