;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-json-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-json-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-json-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh JSON mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'json-mode)
;; self define javascript mode here!
(defun jcs-json-mode-hook ()

  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)

  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2)

  ;; enable the stuff you want for JavaScript here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (defun jcs-json-format()
    (interactive)

    ;; empty, cause json should only take data.
    ;; Comment will be treat as a data too...
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]json" buffer-file-name) (jcs-json-format))
        )

  ;; jcs javascript key binding
  (define-key json-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key json-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key json-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key json-mode-map (kbd "*") 'jcs-c-comment-pair)
  )
(add-hook 'json-mode-hook 'jcs-json-mode-hook)

(add-to-list 'auto-mode-alist '("\\.json?\\'" . json-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-json-mode.el file
