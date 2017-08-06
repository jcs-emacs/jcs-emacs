;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-js-mode.el             -*- Emacs-Lisp -*-

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
;; JenChieh JavaScript mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'ac-js2)
(setq ac-js2-evaluate-calls t)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

(require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(require 'js2-mode)
;; self define javascript mode here!
(defun jcs-javascript-mode-hook ()

  ;; enable impatient mode for real time editing.
  (impatient-mode t)

  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)

  ;; enable the stuff you want for JavaScript here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (defun jcs-javascript-format()
    (interactive)

    ;; define macro
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

    ;; insert the file info header.
    (jcs-global-file-info)

    ;; do JavaScript specific thing here...
    (insert "\n\n") ;; currently nothing.

    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]js" buffer-file-name) (jcs-javascript-format))
        )

  ;; jcs javascript key binding
  (define-key js2-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key js2-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key ac-js2-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key js2-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key js2-mode-map (kbd "*") 'jcs-c-comment-pair)
  )
(add-hook 'js2-mode-hook 'jcs-javascript-mode-hook)

(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-js-mode.el file
