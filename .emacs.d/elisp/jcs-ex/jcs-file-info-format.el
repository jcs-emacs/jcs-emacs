;; This is the start of jcs-file-info-format.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-file-info-format.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-file-info-format is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-file-info-format is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;========================================
;;      JENCHIEH KEY GLOBAL INFO
;;----------------------------------

(defun jcs-parse-ini (filePath)
  "Parse a .ini file.
FILEPATH : .ini file to parse."

  (let ((tmp-ini (get-string-from-file filePath))
        (tmp-ini-list '())
        (tmp-pair-list nil)
        (tmp-keyword "")
        (tmp-value "")
        (count 0))
    (setq tmp-ini (split-string tmp-ini "\n"))

    (dolist (tmp-line tmp-ini)
      ;; check not comment.
      (when (not (string-match-p "#" tmp-line))
        ;; Split it.
        (setq tmp-pair-list (split-string tmp-line "="))

        ;; Assign to temporary variables.
        (setq tmp-keyword (nth 0 tmp-pair-list))
        (setq tmp-value (nth 1 tmp-pair-list))

        ;; Check empty value.
        (when (and (not (string= tmp-keyword ""))
                   (not (equal tmp-value nil)))
          (add-to-list 'tmp-ini-list tmp-keyword)
          (add-to-list 'tmp-ini-list tmp-value)))
      (setq count (1+ count)))

    ;; Reverse list once.
    (setq tmp-ini-list (reverse tmp-ini-list))

    ;; return list.
    tmp-ini-list))

(defun jcs-swap-keyword-template (template-str)
  "Swap all keyword in template to proper information.
TEMPLATE-STR : template string data."
  (let ((tmp-ini-list '())
        (tmp-keyword "")
        (tmp-value "")
        (tmp-index 0))

    ;; parse and get the list of keyword and value.
    (setq tmp-ini-list (jcs-parse-ini "~/.emacs.d/elisp/jcs-ex/ex-template/template_config.properties"))

    (while (< tmp-index (length tmp-ini-list))

      (setq tmp-keyword (nth tmp-index tmp-ini-list))
      (setq tmp-value (nth (1+ tmp-index) tmp-ini-list))

      ;; Add `#' infront and behind the keyword.
      ;; For instance, `CREATOR' -> `#CREATOR#'.
      (setq tmp-keyword (concat "#" tmp-keyword))
      (setq tmp-keyword (concat tmp-keyword "#"))

      ;; Check if the value is a function?
      (if (string-match-p "(" tmp-value)
          (progn
            ;; Remove `(' and `)', if is a function.
            (setq tmp-value (s-replace "(" "" tmp-value))
            (setq tmp-value (s-replace ")" "" tmp-value))
            ;; Call the `tmp-value' with a function.
            (setq template-str (s-replace tmp-keyword
                                          (funcall (intern tmp-value))
                                          template-str)))
        (progn
          ;; Replace it normally with a string.
          (setq template-str (s-replace tmp-keyword
                                        tmp-value
                                        template-str))))
      (setq tmp-index (+ tmp-index 2))))

  ;; return itself.
  template-str)

(defun jcs-insert-template-by-file-path (filePath)
  "Swap all keywords then insert it to current buffer.
FILEPATH : file path to insert and swap keyword."
  (let ((template-str (get-string-from-file filePath)))
    (setq template-str (jcs-swap-keyword-template template-str))
    (insert template-str)))

;;---------------------------------------------
;; Full File info design here...
;; general comment style.
;;---------------------------------------------
(defun jcs-global-file-info ()
  "Useing '/*' '*/' for commenting programming languages."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/global_template.txt"))

;;---------------------------------------------
;; Tag file comment style
;;---------------------------------------------
(defun jcs-tag-file-info ()
  "Tag file header info for tag language."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/tag_template.txt"))

;;---------------------------------------------
;; Manage file comment style
;;---------------------------------------------
(defun jcs-manage-file-info ()
  "Any managing file format.
Text file, batch file, shell script, etc."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/manage_template.txt"))

;;---------------------------------------------
;; Asm file comment style
;;---------------------------------------------
(defun jcs-asm-file-format ()
  "Specific header format for Assembly Language/lisp/elisp, etc."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/asm_template.txt"))

;;---------------------------------------------
;; Specialize the makefile format more specific.
;;---------------------------------------------
(defun jcs-makefile-format-info ()
  "File header format specific for makefile depends \
on language selected."
  (call-interactively 'jcs-ask-makefile-language))

(defun jcs-makefile-cc-app-template ()
  "Default makefile template for normal application."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/makefile_cc_app.txt"))

(defun jcs-makefile-cc-lib-template ()
  "Library makefile template for static library or shared library."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/makefile_cc_lib.txt"))

;;---------------------------------------------
;; Specialize the CMakeLists to more specific.
;;---------------------------------------------
(defun jcs-cmake-format-info ()
  "CMake file format info."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cmake_template.txt"))

;;---------------------------------------------
;; Lua file header format.
;;---------------------------------------------
(defun jcs-lua-file-format-info ()
  "Lua file header format."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/lua_template.txt"))

;;---------------------------------------------
;; Batch file header format.
;;---------------------------------------------
(defun jcs-batch-file-format-info ()
  "Header format for batch file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/batch_template.txt"))

;;---------------------------------------------
;; C Header file format.
;;---------------------------------------------
(defun jcs-c-header-file-format-info ()
  "Header for C header file."
  (jcs-global-file-info))

;;---------------------------------------------
;; C Source file format.
;;---------------------------------------------
(defun jcs-c-source-file-format-info ()
  "Header for C source file."
  (jcs-global-file-info))

;;---------------------------------------------
;; C++ Header file format.
;;---------------------------------------------
(defun jcs-c++-header-file-format-info ()
  "Header for C++ header file."

  (insert "#ifndef __")
  (push-mark)
  (insert (jcs-get-file-name))
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H__\n")
  (jcs-global-file-info)
  (insert "#define __")
  (push-mark)
  (insert (jcs-get-file-name))
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H__")
  (insert "\n\n\n")

  ;; >>>> Method 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; Just add the template no matter what.
  ;;(jcs-c++-default-header-template)
  ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; OR
  ;; >>>> Method 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; Ask to add c++ template.
  (call-interactively 'jcs-ask-cpp-default-header)
  ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  (insert "#endif /* __")
  (push-mark)
  (insert (jcs-get-file-name))
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H__ */\n")
  )

;;---------------------------------------------
;; C++ Source file format.
;;---------------------------------------------
(defun jcs-c++-source-file-format-info ()
  "Header for C++ source file."
  (jcs-global-file-info)

  ;; >>>> Method 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; Just add the template no matter what.
  ;;(jcs-c++-default-source-template)
  ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; OR
  ;; >>>> Method 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ;; Ask to add c++ template.
  (call-interactively 'jcs-ask-cpp-default-source)
  ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  )

;;---------------------------------------------
;; C++ Default Header and Source Template.
;;---------------------------------------------
(defun jcs-c++-default-header-template ()
  "C++ Default Header Constrcutor and Destructor."

  ;; insert class
  (insert "/**\n")
  (insert " * @class ")
  (insert (jcs-get-file-name))
  (insert "\n")
  (insert " * @brief Class description...\n")
  (insert " */\n")
  (insert "class ")
  (insert (jcs-get-file-name))
  (insert "\n{\n")
  (insert "private:\n\n")
  (insert "public:\n")

  ;; constructor & destructor.
  (insert (jcs-get-file-name))
  (insert "();\n")
  (insert "~")
  (insert (jcs-get-file-name))
  (insert "();\n\n\n")

  (insert "    /* operator */\n\n")
  (insert "    /* setter */\n\n")
  (insert "    /* getter */\n")

  (insert "\n};")
  (insert "\n\n"))

(defun jcs-c++-default-source-template ()
  "C++ Default Source Constrcutor and Destructor."

  (insert "\n")
  (insert "#include \"")
  (insert (jcs-get-file-name))
  (insert ".h\"\n\n\n")

  ;; insert constructor
  (insert (jcs-get-file-name))
  (insert "::")
  (insert (jcs-get-file-name))
  (insert "()\n")
  (insert "{\n\n")
  (insert "}\n\n")

  ;; insert destructor
  (insert (jcs-get-file-name))
  (insert "::~")
  (insert (jcs-get-file-name))
  (insert "()\n")
  (insert "{\n\n")
  (insert "}\n")
  )

;;---------------------------------------------
;; COBOL file header format.
;;---------------------------------------------
(defun jcs-cobol-file-format-info ()
  "Header format for COBOL."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cobol_template.txt"))

;;---------------------------------------------
;; HTML file header format.
;;---------------------------------------------
(defun jcs-html-file-format-info ()
  "Header format for COBOL."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/html_template.txt"))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-file-info-format.el file
