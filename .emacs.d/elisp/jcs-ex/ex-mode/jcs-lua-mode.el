;; This is the start of jcs-lua-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-lua-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2017>
;; Time-stamp: <2017-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-lua-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-lua-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Lua mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'lua-mode)
(defun jcs-lua-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-lua-script-format ()
    "Format the given file as a class. - JenChieh Lua Script"
    (interactive)

    (if (is-current-file-empty-p)
        (progn
          (jcs-lua-file-format-info)
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]lua" buffer-file-name) (jcs-lua-script-format))
        ((string-match "[.]luac" buffer-file-name) (jcs-lua-script-format))
        )

  ;; jcs Lua key binding
  (define-key lua-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key lua-mode-map "\C-c\C-c" 'kill-ring-save)

  )
(add-hook 'lua-mode-hook 'jcs-lua-mode-hook)

(add-to-list 'auto-mode-alist '("\\.lua?\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.luac?\\'" . lua-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-lua-mode.el file
