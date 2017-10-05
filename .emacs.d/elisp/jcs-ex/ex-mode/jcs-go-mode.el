;; This is the start of jcs-go-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-go-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Wed Oct 05 13:51:49 EST 2016>
;; Time-stamp: <2017-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-go-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-go-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Lua mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'go-mode)
(defun jcs-go-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (defun jcs-go-script-format ()
    "Format the given file as a class. - JenChieh GO Script"
    (interactive)

    (if (is-current-file-empty-p)
        (progn
          ;; macro
          (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
          (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

          (jcs-global-file-info)
          (insert "\n\n")
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]go" buffer-file-name) (jcs-go-script-format))
        )

  ;; jcs Lua key binding
  (define-key go-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key go-mode-map "\C-c\C-c" 'kill-ring-save)

  )
(add-hook 'go-mode-hook 'jcs-go-mode-hook)

(add-to-list 'auto-mode-alist '("\\.go?\\'" . go-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-go-mode.el file
