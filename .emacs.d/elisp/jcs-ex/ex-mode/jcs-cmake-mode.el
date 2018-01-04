;; This is the start of jcs-cmake-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-cmake-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-cmake-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-cmake-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh CMake mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'cmake-mode)
(defun jcs-cmake-mode-hook ()
  ;;
  (electric-pair-mode nil)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-makefile-format ()
    "Format the given file as a makefile. - JenChieh Makefile"

    (if (is-current-file-empty-p)
        (progn

          (jcs-manage-file-info)
          (jcs-makefile-format-info)

          (beginning-of-buffer)
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]makefile" buffer-file-name) (jcs-makefile-format))
        ((string-match "[Mm]akefile" buffer-file-name) (jcs-makefile-format))
        ((string-match "[.]mak" buffer-file-name) (jcs-makefile-format))
        )

  (defun jcs-cmake-format ()
    "Format the given file as a CMakeLists. - JenChieh CMake"
    (jcs-manage-file-info)
    (jcs-cmake-format-info)

    (beginning-of-buffer)
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "CMakeLists.txt" buffer-file-name) (jcs-cmake-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs makefile key binding
  (define-key cmake-mode-map (kbd "<up>") 'jcs-py-indent-up)
  (define-key cmake-mode-map (kbd "<down>") 'jcs-py-indent-down)
  (define-key cmake-mode-map (kbd "RET") 'jcs-makefile-newline)

  (define-key cmake-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key cmake-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; tabify save key
  (define-key cmake-mode-map "\C-s" 'jcs-tabify-save-buffer)

  ;; Edit
  (define-key cmake-mode-map (kbd "SPC") 'jcs-py-space)
  (define-key cmake-mode-map (kbd "S-SPC") 'jcs-py-real-space)
  (define-key cmake-mode-map (kbd "<backspace>") 'jcs-py-backspace)
  (define-key cmake-mode-map (kbd "S-<backspace>") 'jcs-py-real-backspace)
  )
(add-hook 'cmake-mode-hook 'jcs-cmake-mode-hook)

(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)CMakeLists.txt" . cmake-mode))

;; temporary makefile
(add-to-list 'auto-mode-alist '("\\.mak?\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.makfile?\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)[Mm]akefile" . cmake-mode))

;; For autotools, autoconf, automake.
(add-to-list 'auto-mode-alist '("\\.ac?\\'" . cmake-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cmake-mode.el file
