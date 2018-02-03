;; This is the start of jcs-actionscript-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-actionscript-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-actionscript-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-actionscript-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh ActionScript 3.0 mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'actionscript-mode)
(defun jcs-actionscript-mode-hook ()
  "JayCeS AS mode."

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-actionsript-class-format ()
    "Format the given file as a class. - JenChieh AS class"

    (if (is-current-file-empty-p)
        (progn
          (jcs-global-file-info)

          (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

          (insert "\n\n")
          (insert "public class ")
          (push-mark)
          (insert BaseFileName)
          (pop-mark)
          (insert " {\n\n}\n\n")
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]as" buffer-file-name) (jcs-actionsript-class-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs java key binding
  (define-key actionscript-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key actionscript-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key actionscript-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key actionscript-mode-map (kbd "*") 'jcs-c-comment-pair)
  )

(add-hook 'actionscript-mode-hook 'jcs-actionscript-mode-hook)

(add-to-list 'auto-mode-alist '("\\.as?\\'" . actionscript-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-actionscript-mode.el file
