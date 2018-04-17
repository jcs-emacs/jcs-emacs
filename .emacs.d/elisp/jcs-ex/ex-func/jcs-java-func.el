;; This is the start of jcs-java-func.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-java-func.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sat Apr 07 15:09:05 EST 2018>
;; Time-stamp: <2018-04-07 15:09:05>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2018 Jen-Chieh Shen

;; jcs-java-func is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-java-func is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the Java related file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Code:

(defvar jcs-java-source-dir-name "src"
  "Java source directory name.")

(defun jcs-java-insert-package-from-src ()
  "Insert package string from `src' directory."
  (let ((tmp-file-name (buffer-file-name))
        (tmp-split-dir-string '())
        (tmp-split-dir-string-size -1)
        (tmp-insert-package-path-string ""))
    ;; See if `src' directory contain in the string.
    (when (jcs-contain-string jcs-java-source-dir-name tmp-file-name)

      ;; split the string by using string -> `src'.
      (setq tmp-split-dir-string (split-string tmp-file-name jcs-java-source-dir-name))

      ;; get the length of the split string
      (setq tmp-split-dir-string-size (length tmp-split-dir-string))

      ;; NOTE(jenchieh): Get the last part of the path.
      ;; If there are two `src' directory in the path.
      ;; We will just like to get the last part of the file path
      ;; and ignore whatever infront of them.
      (setq tmp-insert-package-path-string
            ;; Get the last part of the file path array.
            (nth (1- tmp-split-dir-string-size) tmp-split-dir-string))

      ;; Now we remove the file name from last part of path..
      (setq tmp-insert-package-path-string
            (jcs-remove-string-by-substring tmp-insert-package-path-string
                                            (jcs-get-file-name)))

      ;; NOTE(jenchieh): If the string is less than two.
      ;; Meaning the file is directly under `default package'/`src'
      ;; directory.
      (when (<= 2 (length tmp-insert-package-path-string))
        ;; Replace all `/' to `.'.
        (setq tmp-insert-package-path-string (jcs-replace-string
                                              "/"
                                              "."
                                              tmp-insert-package-path-string))

        ;; Remove the first and last character -> `.'.
        (setq tmp-insert-package-path-string
              (subseq tmp-insert-package-path-string
                      ;; Starting position, in order to remove the
                      ;; first character we start the string from
                      ;; index 1 instead of 0.
                      1
                      ;; End position, simply just minus one from
                      ;; length.
                      (1- (length tmp-insert-package-path-string))))

        ;; Add a `;' semi-colon at the end of the ready insert string.
        (setq tmp-insert-package-path-string
              (concatenate 'string tmp-insert-package-path-string ";"))

        ;; Insert the final package string.
        (insert "package ")
        (insert tmp-insert-package-path-string)
        (insert "\n")))))

;;;###autoload
(defun jcs-java-insert-package-src ()
  "Insert the package source declaration."
  (interactive)
  (save-excursion
    ;; Goto the beginning of the buffer.
    (goto-char (point-min))

    ;; Check if the first word package.
    (when (current-word-equal-p "package")
      ;; kill the old package source declaration.
      (jcs-kill-whole-line))

    ;; insert the new one.
    (jcs-java-insert-package-from-src)

    (jcs-keep-one-line-between)))

;;;###autoload
(defun jcs-java-organize-imports ()
  "Organize all the import package.
Including adding or removing the package path."
  (interactive)
  ;; first organize package declaration.
  (jcs-java-insert-package-src)

  ;; TODO(jenchieh): Organize the imports..
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-java-func.el file
