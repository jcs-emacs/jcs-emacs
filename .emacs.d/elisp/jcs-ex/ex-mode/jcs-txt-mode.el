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


;;; Code:

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Text mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;====================================
;; normal text mode
;;---------------------------
(require 'gitignore-mode)
(defun jcs-gitignore-mode-hook ()
  "JayCeS Gitignore mode."

  (electric-pair-mode nil)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

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
  (define-key gitignore-mode-map (kbd "<up>") 'previous-line)
  (define-key gitignore-mode-map (kbd "<down>") 'next-line)
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
(require 'org)

;; No fold when open `org' file.
(setq org-startup-folded nil)

(defun jcs-org-mode()
  "JayCeS org mode."

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

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
  (define-key org-mode-map [tab] 'jcs-tab-key)
  (define-key org-mode-map [C-tab] 'org-cycle)

  (define-key org-mode-map (kbd "S-<up>") 'jcs-org-table-up)
  (define-key org-mode-map (kbd "S-<down>") 'jcs-org-table-down)
  (define-key org-mode-map (kbd "S-<left>") 'jcs-org-table-left)
  (define-key org-mode-map (kbd "S-<right>") 'jcs-org-table-right)
  )
(add-hook 'org-mode-hook 'jcs-org-mode)

;; set the defualt text mode to org mode.
(add-to-list 'auto-mode-alist '("\\.txt?\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)README" . org-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)LICENSE" . org-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)bochsrc" . org-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-txt-mode.el file
