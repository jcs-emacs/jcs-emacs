;; This is the start of jcs-after-init.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-after-init.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Thu Aug 04 13:51:49 EST 2017>
;; Time-stamp: <2017-08-04 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-after-init is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-after-init is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Do stuff after initialize.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Override all the mode's key bindings.
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-global-key.el")

;;
;; All hook listed.
;; URL(jenchieh): https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
;;

;;;###autoload
(defun jcs-focus-in-hook ()
  "When window is focus."
  )

;;;###autoload
(defun jcs-focus-out-hook ()
  "When window is not focus."
  )

(add-hook 'focus-in-hook 'jcs-focus-in-hook)
(add-hook 'focus-out-hook 'jcs-focus-out-hook)

;;;###autoload
(defun jcs-find-file-hook ()
  "When temporary buffer shown."

  (save-selected-window
    (ignore-errors
      (jcs-jump-to-window "*Buffer List*"))
    (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
      (jcs-buffer-menu)))
  )
(add-hook 'find-file-hook 'jcs-find-file-hook)

;;; Diminish
;; NOTE(jenchieh): Do not show theses modes in the mode line.
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'auto-complete-mode)
(diminish 'company-mode)
(diminish 'flycheck-mode)
(diminish 'flymake-mode)
(diminish 'helm-mode)
(diminish 'helm-gtags-mode)
(diminish 'impatient-mode)
(diminish 'js2-refactor-mode)
(diminish 'js2r)
(diminish 'outline-minor-mode)
(diminish 'skewer-mode)
(diminish 'yas-minor-mode)
(diminish 'auto-highlight-symbol-mode)


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-after-init.el file
