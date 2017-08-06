;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-python-mode.el             -*- Emacs-Lisp -*-

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
;; JenChieh Python mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'python-mode)
(defun jcs-python-mode-hook ()
  ;; enable the stuff you want for Python here
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (defun jcs-python-class-format ()
    "Format the given file as a class. - JenChieh Python class"

    (jcs-manage-file-info)

    ;; define macro
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

    (insert "class ")
    (insert BaseFileName)
    (insert "(object):\n\n")

    (insert "    \"\"\"TODO(jenchieh): Class Description here...\"\"\"")
    (insert "\n\n")

    (insert "    #*********************************************#\n")
    (insert "    #*             Public Variables              *#\n")
    (insert "    #*********************************************#\n\n")

    (insert "    #*********************************************#\n")
    (insert "    #              Private Variables             *#\n")
    (insert "    #*********************************************#\n\n")

    (insert "    #*********************************************#\n")
    (insert "    #              Protected Variables           *#\n")
    (insert "    #*********************************************#\n\n")

    (insert "    #*********************************************#\n")
    (insert "    #                Constructor                 *#\n")
    (insert "    #*********************************************#\n")
    (insert "    def __init__(self):\n")
    (insert "        \"\"\"Constructor.\"\"\"\n\n")

    (insert "    #====================\n")
    (insert "    # Public Methods\n\n")

    (insert "    #====================\n")
    (insert "    # Protected Methods\n\n")

    (insert "    #====================\n")
    (insert "    # Private Methods\n\n")

    (insert "    #====================\n")
    (insert "    # setter / getter\n\n")

    ;; Move to beginning of the buffer.
    (beginning-of-buffer)
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]py" buffer-file-name) (jcs-python-class-format))
        )

  ;; jcs java key binding
  (define-key python-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key python-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key python-mode-map [C-backspace] 'jcs-backward-delete-word)

  (define-key python-mode-map [M-up] 'previous-blank-line)
  (define-key python-mode-map [M-down] 'next-blank-line)

  (define-key python-mode-map "\C-k\C-f" 'jcs-py-indent-region)
  (define-key python-mode-map "\C-k\C-d" 'jcs-py-format-document)
  (define-key python-mode-map (kbd "C-S-f") 'jcs-py-format-region-or-document)

  ;; Edit
  (define-key python-mode-map (kbd "<up>") 'jcs-py-indent-up)
  (define-key python-mode-map (kbd "<down>") 'jcs-py-indent-down)
  (define-key python-mode-map (kbd "SPC") 'jcs-py-space)
  )
(add-hook 'python-mode-hook 'jcs-python-mode-hook)

(add-to-list 'auto-mode-alist '("\\.py?\\'" . python-mode))

(require 'elpy)
;;(elpy-enable)

(require 'ein)
;;(elpy-use-ipython)

;; enable autopep8 formatting on save
(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-python-mode.el file
