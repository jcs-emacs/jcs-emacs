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


(require 'vimrc-mode)
(defun jcs-vim-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)


  (defun jcs-vim-script-format ()
    "Format the given file as a class. - JenChieh Lua Script"
    (when (is-current-file-empty-p)
      (jcs-insert-vimscript-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]vim" buffer-file-name) (jcs-vim-script-format))
        ((string-match "[.]vimrc" buffer-file-name) (jcs-vim-script-format))
        ((string-match "_vimrc" buffer-file-name) (jcs-vim-script-format))
        )

  ;; jcs vim mode key binding
  (define-key vimrc-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key vimrc-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key vimrc-mode-map "\C-a" 'mark-whole-buffer)

  (define-key vimrc-mode-map (kbd "<up>") 'jcs-previous-line)
  (define-key vimrc-mode-map (kbd "<down>") 'jcs-next-line)
  )
(add-hook 'vimrc-mode-hook 'jcs-vim-mode-hook)

(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)_vimrc" . vimrc-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-vimscript-mode.el file
