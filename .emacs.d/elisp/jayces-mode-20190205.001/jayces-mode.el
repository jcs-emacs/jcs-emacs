;;; jayces-mode.el --- Major mode for editing JayCeS file.                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-10-11 16:28:04

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Major mode for editing JayCeS file
;; Keyword: jayces, major mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs090218/jayces-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mode for editing JayCeS file.
;;

;;; Code:

(eval-and-compile
  (require 'compile)
  (require 'cc-mode)
  (require 'font-lock)
  (require 'rx)
  (require 'newcomment))

(eval-when-compile
  (require 'cl-lib))


;;; Font Lock
(defconst jayces--font-lock-keywords
  '(("function" . font-lock-keyword-face))
  "Font lock keywords for `jayces-mode'.  See `font-lock-keywords'.")


(defvar jayces-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `jayces-mode'.")


(defvar jayces-mode-map nil
  "Kaymap for `jayces-mode'.")


(unless jayces-mode-map
  (progn
    (setq jayces-mode-map (make-sparse-keymap))

    ;; keymap define here.
    (define-key jayces-mode-map (kbd "RET") 'jcs-smart-context-line-break)
    (define-key jayces-mode-map (kbd "*") 'jcs-c-comment-pair)
    ))

;; define hook
(defcustom jayces-mode-hook nil
  "*Hook to be run when `jayces-mode' is entered."
  :group 'jayces
  :type  'hook)

;; main JayCeS code mode.
(define-derived-mode jayces-mode prog-mode "JayCeS"
  "Major mode for editing JayCeS file."

  :group 'jayces
  :syntax-table jayces-mode-syntax-table

  (setq-local font-lock-defaults (list jayces--font-lock-keywords))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (let ((c-buffer-is-cc-mode t))
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))


  ;; bind keymap
  (use-local-map jayces-mode-map)
  )

(add-to-list 'auto-mode-alist '("\\.jcs$" . jayces-mode))
(add-to-list 'auto-mode-alist '("\\.jayces$" . jayces-mode))

(provide 'jayces-mode)
;;; jayces-mode.el ends here
