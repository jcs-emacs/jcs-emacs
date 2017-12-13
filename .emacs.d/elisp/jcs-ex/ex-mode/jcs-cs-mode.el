;; This is the start of jcs-cs-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-cs-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-cs-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-cs-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh C# mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'csharp-mode)
(defun jcs-csharp-mode-hook ()

  (preproc-font-lock-mode t)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-csharp-class-format ()
    "Format the given file as a class. - JenChieh C# class"

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
        ((string-match "[.]cs" buffer-file-name) (jcs-csharp-class-format))
        )

  ;; Set Faces.
  (face-remap-add-relative 'font-lock-comment-face '(jcs-font-lock-comment-face))
  (face-remap-add-relative 'font-lock-string-face '(jcs-font-lock-string-face))

  ;; jcs C# key binding
  (define-key csharp-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key csharp-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key csharp-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key csharp-mode-map (kbd "*") 'jcs-c-comment-pair)

  (define-key csharp-mode-map (kbd "/") 'jcs-vs-csharp-maybe-insert-codedoc)

  (define-key csharp-mode-map "\eq" 'jcs-other-window-prev)
  )
(add-hook 'csharp-mode-hook 'jcs-csharp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cs?\\'" . csharp-mode))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cs-mode.el file
