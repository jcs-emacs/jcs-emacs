;; This is the start of jcs-cs-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-cs-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Tue Nov 28 13:51:49 EST 2017>
;; Time-stamp: <2017-11-28 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-cs-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-cs-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the C# related file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Code:

(defun jcs-vs-csharp-do-doc-string ()
  "Check if should insert the doc string by checking only \
comment character on the same line."

  (let ((do-doc-string t))
    (jcs-goto-first-char-in-line)

    (while (not (is-end-of-line-p))
      (forward-char 1)
      (when (and (not (current-char-equal-p " "))
                 (not (current-char-equal-p "\t"))
                 (not (current-char-equal-p "/")))
        ;; return false.
        (setq do-doc-string nil)
        (equal do-doc-string t)))

    ;; return true.
    (equal do-doc-string t)))

(defun jcs-vs-csharp-maybe-insert-codedoc ()
  "Insert comment like Visual Studio comment style.

URL(jenchieh): https://github.com/josteink/csharp-mode/issues/123"
  (interactive)

  (insert "/")

  (let ((active-comment nil)
        (next-line-not-empty nil))
    (save-excursion
      (backward-char 1)
      (when (current-char-equal-p "/")
        (backward-char 1)
        (when (current-char-equal-p "/")
          (backward-char 1)
          (when (not (current-char-equal-p "/"))
            (when (jcs-vs-csharp-do-doc-string)
              (setq active-comment t)))))

      ;; check if next line empty.
      (jcs-next-line)
      (when (not (current-line-empty-p))
        (setq next-line-not-empty t)))


    (when (and (equal active-comment t)
               (equal next-line-not-empty t))
      (insert " <summary>\n")
      (insert "/// \n")
      (insert "/// </summary>")

      (jcs-smart-indent-up)
      (jcs-smart-indent-down)
      (jcs-smart-indent-up)
      (end-of-line)

      ;; Check other comment type.
      ;; ex: param, returns, etc.
      (save-excursion
        ;; Goto the function line before insert doc string.
        (jcs-next-line)
        (jcs-next-line)

        ;; insert comment doc comment string.
        (jcs-insert-comment-style-by-current-line)
        ))
    ))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-cs-func.el file
