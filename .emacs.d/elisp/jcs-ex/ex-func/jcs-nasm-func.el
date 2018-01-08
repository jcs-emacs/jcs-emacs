;; This is the start of jcs-nasm-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-nasm-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sat Dec 31 13:51:49 EST 2017>
;; Time-stamp: <2017-12-31 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-nasm-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-nasm-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; First load the mode to prevent overwrite after.
(require 'nasm-mode)

(defun nasm-indent-line ()
  "Indent current line as NASM assembly code."
  (interactive)
  (let ((orig (- (point-max) (point))))
    (back-to-indentation)
    (if (or (looking-at (nasm--opt nasm-directives))
            (looking-at (nasm--opt nasm-pp-directives))
            (looking-at "\\[")
            (looking-at ";;+")
            ;;--------------------------------------------------
            ;; ATTENTION(jenchieh): Self copy this function from
            ;; source code. Add these rules for my own use.
            (jcs-is-nasm-indent)
            ;;--------------------------------------------------
            (looking-at nasm-label-regexp))
        (indent-line-to 0)
      (indent-line-to nasm-basic-offset))
    (when (> (- (point-max) orig) (point))
      (goto-char (- (point-max) orig)))))

(defun jcs-is-nasm-indent ()
  "JayCeS's own indent nasm rules.
@return boolean : true - do indent, false - vice versa."
  (let ((do-indent nil))
    (save-excursion
      ;; Goto the first character of current line.
      (back-to-indentation-or-beginning)
      (when (is-beginning-of-line-p)
        (back-to-indentation-or-beginning))
      (forward-char 1)

      ;; Check rule here..
      (when (current-char-equal-p ".")
        (setq do-indent t)))

    (equal do-indent t)))

;;;###autoload
(defun jcs-nasm-return ()
  "Return key for `nasm-mode'."
  (interactive)

  (let ((continue-comment nil))
    (save-excursion
      (ignore-errors
        (jcs-goto-first-char-in-line)

        (forward-char 1)
        (when (current-char-equal-p ";")
          (forward-char 1)
          (when (current-char-equal-p ";")
            (setq continue-comment t)))))

    (newline-and-indent)

    (when (equal continue-comment t)
      (insert ";; ")
      (save-excursion
        (indent-line-to 0)))))

;;;###autoload
(defun jcs-nasm-comment ()
  "Comment key for `nasm-mode'."
  (interactive)

  ;; Call normal nasm comment function before do our
  ;; own nasm comment.
  (call-interactively 'nasm-comment)

  (let ((should-indent nil))
    (save-excursion
      (backward-char 1)
      (when (current-char-equal-p ";")
        (backward-char 1)
        (when (is-met-first-char-at-line-p)
          (setq should-indent t)
          ;; Indent it to the very left/beginning of line.
          (indent-line-to 0))))

    (save-excursion
      ;; If search backward failed, try forward.
      (when (equal should-indent nil)
        (forward-char 1)
        (when (current-char-equal-p ";")
          (setq should-indent t)
          ;; Indent it to the very left/beginning of line.
          (indent-line-to 0))))

    (when (and (equal should-indent t)
               (is-end-of-line-p))
      (insert " "))))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-nasm-func.el file
