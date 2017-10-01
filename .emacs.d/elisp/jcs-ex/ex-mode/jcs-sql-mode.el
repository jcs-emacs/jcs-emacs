;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-sql-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Sun Sep 17 13:51:49 EST 2017>
;; Time-stamp: <2017-09-25 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-sql-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-sql-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh SQL mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'sql-indent)
;; URL(jenchieh): https://www.emacswiki.org/emacs/SqlIndent
;; 1 = 2 spaces,
;; 2 = 4 spaces,
;; 3 = 6 spaces,
;; n = n * 2 spaces,
;; etc.
(setq sql-indent-offset 1)

(require 'sql)
(defun jcs-sql-mode-hook()
  "Add hooks to `sql-mode' hook."
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  (defun jcs-sql-format ()
    "File format for editing SQL file."
    (interactive)

    (if (is-current-file-empty-p)
        (progn
          ;; macro
          (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
          (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

          (insert "/*\n")

          (insert "Service Platform Data Transfer\n")
          (insert "\n")

          (insert "Source Server         : $server_name$\n")
          (insert "Source Server Type    : $server_type$\n")
          (insert "Source Server Version : $server_version$\n")
          (insert "Source Host           : $hostname$\n")
          (insert "Source Schema         : $schema$\n")
          (insert "\n")

          (insert "Target Server Type    : $server_type$\n")
          (insert "Target Server Version : $server_version$\n")
          (insert "File Encoding         : $file_encoding$\n")
          (insert "\n")

          (insert "Date: ")
          (jcs-timestamp-ver3)
          (insert "\n")

          (insert "*/\n")
          (insert "\n\n")
          ))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]sql" buffer-file-name) (jcs-sql-format))
        )

  ;; Edit
  (define-key sql-mode-map (kbd "<up>") 'jcs-smart-indent-up)
  (define-key sql-mode-map (kbd "<down>") 'jcs-smart-indent-down)

  (define-key sql-mode-map "\C-c\C-c" 'kill-ring-save)
  )

(add-hook 'sql-mode-hook 'jcs-sql-mode-hook)

(add-to-list 'auto-mode-alist '("\\.sql?\\'" . sql-mode))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-sql-mode.el file
