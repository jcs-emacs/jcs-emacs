;; This is the start of jcs-batch-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-batch-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-batch-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-batch-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Batch mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'bat-mode)
(defun jcs-batch-mode-hook ()
  ;;
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-batch-script-format ()
    "Format the given file as a class. - JenChieh Batch Script"
    (when (is-current-file-empty-p)
      (jcs-insert-batch-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]bat" buffer-file-name) (jcs-batch-script-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs key binding
  (define-key bat-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key bat-mode-map "\C-c\C-c" 'kill-ring-save)

  (define-key bat-mode-map (kbd "<up>") 'jcs-previous-line)
  (define-key bat-mode-map (kbd "<down>") 'jcs-next-line)
  )
(add-hook 'bat-mode-hook 'jcs-batch-mode-hook)

(add-to-list 'auto-mode-alist '("\\.bat?\\'" . bat-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-batch-mode.el file
