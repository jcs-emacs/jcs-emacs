;; This is the start of jcs-java-mode.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-java-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-java-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-java-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Java mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;;; Java Imports
(require 'java-imports)
;; See customization below for where to put java imports
(setq
 java-imports-find-block-function
 'java-imports-find-place-sorted-block)

(add-hook 'jdee-mode-hook 'java-imports-scan-file)

;;; Minor mode for Java Development
;; SOURCE: https://github.com/mopemope/meghanada-emacs
(require 'meghanada)

(require 'jdee)
(defun jcs-java-mode-hook ()

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;;; `meghanada' Configuration
  ;;(meghanada-mode t)


  (defun jcs-java-class-format ()
    "Format the given file as a class. - JenChieh Java class"

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
          )
      ))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]java" buffer-file-name) (jcs-java-class-format))
        )

  ;; jcs java key binding
  (define-key java-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key java-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key java-mode-map (kbd "RET") 'jcs-smart-context-line-break)

  (define-key java-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; switch frame.
  (define-key java-mode-map "\ew" 'jcs-other-window-next)
  (define-key java-mode-map (kbd "M-q") 'jcs-other-window-prev)

  ;; `java-imports'
  (define-key java-mode-map (kbd "C-S-o") 'java-imports-add-import-dwim)
  )
(add-hook 'java-mode-hook 'jcs-java-mode-hook)
(add-to-list 'auto-mode-alist '("\\.java?\\'" . java-mode))

;;(add-hook 'jdee-mode-hook 'jcs-java-mode-hook)
;;(add-to-list 'auto-mode-alist '("\\.java?\\'" . jdee-mode))

;;(autoload 'jde-mode "~/.emacs.d/elpha/jdee-20160304.536/jdee.el" "JDE mode" t)
;; (setq auto-mode-alist
;;       (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-java-mode.el file
