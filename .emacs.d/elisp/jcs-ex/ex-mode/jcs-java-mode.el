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


;;; Minor mode for Java Development
;; SOURCE: https://github.com/mopemope/meghanada-emacs
(require 'meghanada)

(require 'jdee)
(defun jcs-java-mode-hook ()

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;;; `meghanada' Configuration
  ;;(meghanada-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")

  (defun jcs-java-class-format ()
    "Format the given file as a class. - JenChieh Java class"
    (when (is-current-file-empty-p)
      ;; insert the package declaration.
      (jcs-java-insert-package-from-src)

      ;; Leave one empty line between header.
      (insert "\n")

      (jcs-insert-java-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]java" buffer-file-name) (jcs-java-class-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; NOTE(jenchieh): change the face locally to this mode.
  (face-remap-add-relative 'font-lock-constant-face
                           '((:foreground "#D2D2D2")))

  ;; jcs java key binding
  (define-key java-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key java-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key java-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key java-mode-map (kbd "*") 'jcs-c-comment-pair)

  ;; switch frame.
  (define-key java-mode-map "\ew" 'jcs-other-window-next)
  (define-key java-mode-map (kbd "M-q") 'jcs-other-window-prev)

  ;; imports/package declaration.
  (define-key java-mode-map (kbd "C-S-o") 'jcs-java-organize-imports)
  )
(add-hook 'java-mode-hook 'jcs-java-mode-hook)
(add-to-list 'auto-mode-alist '("\\.java?\\'" . java-mode))

;;(add-hook 'jdee-mode-hook 'jcs-java-mode-hook)
;;(add-to-list 'auto-mode-alist '("\\.java?\\'" . jdee-mode))

;;(autoload 'jde-mode "~/.emacs.d/elpha/jdee-20160304.536/jdee.el" "JDE mode" t)
;; (setq auto-mode-alist
;;       (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))


(provide 'jdee-font-lock)
(if (> emacs-major-version 23)
    (defconst c-doc-face-name 'font-lock-doc-face)
  ;; starting with 24, cc-fonts clobbers this because of some change of order
  ;; of loading
  (eval-after-load
      "cc-fonts"
    '(defconst c-doc-face-name 'font-lock-doc-face)))


(set-face-attribute 'jdee-font-lock-number-face nil
                    :foreground "olive drab")

(set-face-attribute 'jdee-font-lock-constant-face nil
                    :foreground "#D2D2D2")

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-java-mode.el file
