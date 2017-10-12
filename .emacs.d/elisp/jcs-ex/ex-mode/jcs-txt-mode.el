;; This is the start of jcs-txt-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-txt-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-txt-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-txt-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Text mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;====================================
;; normal text mode
;;---------------------------
(require 'gitignore-mode)
(defun jcs-gitignore-mode-hook ()
  ;;
  (electric-pair-mode nil)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (defun jcs-text-related-format ()
    "Format the given file as a text related. - JenChieh Text file."

    (jcs-manage-file-info)
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]txt" buffer-file-name) (jcs-text-related-format))
        ((string-match "[.]gitignore" buffer-file-name) (jcs-text-related-format))
        ((string-match "[.]gitattributes" buffer-file-name) (jcs-text-related-format))
        )

  ;; jcs gitignore key binding
  (define-key gitignore-mode-map (kbd "<up>") 'previous-line)
  (define-key gitignore-mode-map (kbd "<down>") 'next-line)
  (define-key gitignore-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key gitignore-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'gitignore-mode-hook 'jcs-gitignore-mode-hook)

;; git file types
(add-to-list 'auto-mode-alist '("\\.gitignore?\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.gitattributes?\\'" . gitignore-mode))

;; temporary mode for all Text related!
(add-to-list 'auto-mode-alist '("\\.bin?\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.md?\\'" . gitignore-mode))

;; temporary ALGOL
(add-to-list 'auto-mode-alist '("\\.alg?\\'" . gitignore-mode))

;; key-binding for normal text mode.
(define-key global-map "\C-xg" 'gitignore-mode)


;;====================================
;; Org mode.
;;---------------------------
(defun jcs-org-mode()

  (defun jcs-org-mode-format()
    "Fromat the given file as a text file related. - JenChieh Text file."

    (if (is-current-file-empty-p)
        (progn
          (jcs-manage-file-info)
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]txt" buffer-file-name) (jcs-org-mode-format))
        )

  ;; jcs org mode key binding
  (define-key org-mode-map (kbd "<up>") 'previous-line)
  (define-key org-mode-map (kbd "<down>") 'next-line)

  (define-key org-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key org-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key org-mode-map "\C-a" 'mark-whole-buffer)
  )
(add-hook 'org-mode-hook 'jcs-org-mode)

;; set the defualt text mode to org mode.
(add-to-list 'auto-mode-alist '("\\.txt?\\'" . org-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-txt-mode.el file
