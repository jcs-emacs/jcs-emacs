;; This is the start of jcs-vimscript-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-vimscript-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2017>
;; Time-stamp: <2017-10-12 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-vimscript-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-vimscript-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh VimScript.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defun jcs-vimscript-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)


  (defun jcs-vimscript-format ()
    "Format the given file as a class. - JenChieh Lua Script"
    (interactive)

    (if (is-current-file-empty-p)
        (progn
          ;; macro
          (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
          (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

          (insert "\"\" ========================================================================\n")
          (insert "\"\" $File: ")
          (insert BaseFileNameWithExtension)
          (insert " $\n")
          (insert "\"\" $Date: ")
          (jcs-timestamp)
          (insert " $\n")
          (insert "\"\" $Revision: $\n")
          (insert "\"\" $Creator: Jen-Chieh Shen $\n")
          (insert "\"\" $Notice: See LICENSE.txt for modification and distribution information $ \n")
          (insert "\"\"                   Copyright (c) ")
          (jcs-year-only)
          (insert " by Shen, Jen-Chieh $\n")
          (insert "\"\" ========================================================================\n")
          (insert "\n\n")
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "_vimrc" buffer-file-name) (jcs-vimscript-format))
        ((string-match "[.]vim" buffer-file-name) (jcs-vimscript-format))
        )

  ;; jcs org mode key binding
  (define-key org-mode-map (kbd "<up>") 'previous-line)
  (define-key org-mode-map (kbd "<down>") 'next-line)

  (define-key org-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key org-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key org-mode-map "\C-a" 'mark-whole-buffer)
  )
(add-hook 'org-mode-hook 'jcs-vimscript-mode-hook)

(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)_vimrc" . org-mode))
(add-to-list 'auto-mode-alist '("\\.vim?\\'" . org-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-vimscript-mode.el file
