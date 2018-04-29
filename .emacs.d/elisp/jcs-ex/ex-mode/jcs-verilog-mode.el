;; This is the start of jcs-verilog-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-verilog-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Thu Aug 04 13:51:49 EST 2017>
;; Time-stamp: <2017-08-04 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-verilog is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-verilog is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Verilog mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(require 'verilog-mode)
(defun jcs-verilog-mode-hook ()
  "Verilog mode"
  (interactive)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-verilog-script-format ()
    "Format the given file as a class. - JenChieh Lua Script"
    (when (is-current-file-empty-p)
      ;; Design format header here...
      ))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]v" buffer-file-name) (jcs-verilog-script-format))
        )


  )
(add-hook 'verilog-mode-hook 'jcs-verilog-mode-hook)

(add-to-list 'auto-mode-alist '("\\.v?\\'" . verilog-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-verilog-mode.el file
