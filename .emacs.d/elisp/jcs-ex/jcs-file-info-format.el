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

      ;; NOTE(jenchieh): Check keyword exist before replacing
      ;; it. Or else it will cause `max-lisp-eval-depth' error.
      (when (string-match-p tmp-keyword template-str)

        ;; Check if the value is a function?
        (if (string-match-p "(" tmp-value)
            (progn
              ;; Remove `(' and `)', if is a function.
              (setq tmp-value (s-replace "(" "" tmp-value))
              (setq tmp-value (s-replace ")" "" tmp-value))

              (setq template-str (s-replace tmp-keyword
                                            (funcall (intern tmp-value))
                                            template-str)))
          (progn
            ;; Replace it normally with a string.
            (setq template-str (s-replace tmp-keyword
                                          tmp-value
                                          template-str)))))
      (setq tmp-index (+ tmp-index 2))))

  ;; return itself.
  template-str)

(defun jcs-get-template-by-file-path (filePath)
  "Replace keyword and return modefieded template string.
FILEPATH : template file."
  (jcs-get-template-by-file-path (get-string-from-file filePath)))

(defun jcs-insert-template-by-file-path (filePath)
  "Swap all keywords then insert it to current buffer.
FILEPATH : file path to insert and swap keyword."
  (let ((template-str (get-string-from-file filePath)))
    (setq template-str (jcs-swap-keyword-template template-str))
    (insert template-str)))


;;---------------------------------------------
;; Buffer String
;;---------------------------------------------

(defvar jcs-preload-global-file-info nil
  "Preload the global file info template.")

(defvar jcs-preload-tag-file-info nil
  "Preload the tag file info template.")

(defvar jcs-preload-manage-file-info nil
  "Preload the manage file info template.")

(defvar jcs-preload-semi-file-info nil
  "Preload the semi file info template.")

(defvar jcs-preload-double-quote-file-info nil
  "Preload the double quote file info template.")

(defvar jcs-preload-double-colon-file-info nil
  "Preload the double colon file info template.")

(defvar jcs-preload-double-dash-file-info nil
  "Preload the double dash file info template.")


;;;###autoload
(defun jcs-reload-file-info ()
  "Reload the template once.
If the template configuration file has change, this must be call
in order to take effect.  Half hot reloading process."
  (interactive)
  (setq jcs-preload-global-file-info (get-string-from-file "~/.emacs.d/elisp/jcs-ex/ex-template/header/global_template.txt"))
  (setq jcs-preload-tag-file-info (get-string-from-file "~/.emacs.d/elisp/jcs-ex/ex-template/header/tag_template.txt"))
  (setq jcs-preload-manage-file-info (get-string-from-file "~/.emacs.d/elisp/jcs-ex/ex-template/header/manage_template.txt"))
  (setq jcs-preload-semi-file-info (get-string-from-file "~/.emacs.d/elisp/jcs-ex/ex-template/header/semi_template.txt"))
  (setq jcs-preload-double-quote-file-info (get-string-from-file "~/.emacs.d/elisp/jcs-ex/ex-template/header/doublequote_template.txt"))
  (setq jcs-preload-double-colon-file-info (get-string-from-file "~/.emacs.d/elisp/jcs-ex/ex-template/header/doublecolon_template.txt"))
  (setq jcs-preload-double-dash-file-info (get-string-from-file "~/.emacs.d/elisp/jcs-ex/ex-template/header/doubledash_template.txt")))

(defun jcs-get-global-file-info()
  "Return the preloaded global file info template."
  (jcs-swap-keyword-template jcs-preload-global-file-info))

(defun jcs-get-tag-file-info()
  "Return the preloaded tag file info template."
  (jcs-swap-keyword-template jcs-preload-tag-file-info))

(defun jcs-get-manage-file-info ()
  "Return the preloaded manage file info template."
  (jcs-swap-keyword-template jcs-preload-manage-file-info))

(defun jcs-get-semi-file-info ()
  "Return the preloaded semi file info template."
  (jcs-swap-keyword-template jcs-preload-semi-file-info))

(defun jcs-get-double-quote-file-info ()
  "Return the preloaded double quote file info template."
  (jcs-swap-keyword-template jcs-preload-double-quote-file-info))

(defun jcs-get-double-colon-file-info ()
  "Return the preloaded double colon file info template."
  (jcs-swap-keyword-template jcs-preload-double-colon-file-info))

(defun jcs-get-double-dash-file-info ()
  "Return the preloaded double dash file info template."
  (jcs-swap-keyword-template jcs-preload-double-dash-file-info))


;;---------------------------------------------
;; Header
;;---------------------------------------------
(defun jcs-insert-global-file-info ()
  "Using '/*' '*/' for commenting programming languages."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/global_template.txt"))

(defun jcs-insert-tag-file-info ()
  "Tag file header info for tag language."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/tag_template.txt"))

(defun jcs-insert-manage-file-info ()
  "Any managing file format.
Text file, batch file, shell script, etc."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/manage_template.txt"))

(defun jcs-insert-semi-file-info ()
  "Specific header format for Assembly Language/lisp/elisp, etc."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/semi_template.txt"))

(defun jcs-insert-double-quote-file-info ()
  "Specific header format for Vim script."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/doublequote_template.txt"))

(defun jcs-insert-double-colon-file-info ()
  "Specific header format for Vim script."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/header/doublecolon_template.txt"))


;;---------------------------------------------
;; Other Template
;;---------------------------------------------

;; Assembly Language
(defun jcs-insert-asm-template ()
  "Header for Assembly Language file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/asm/asm_template.txt"))

;;; ActionScript
(defun jcs-insert-actionscript-template ()
  "Template for ActionScript."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/as/as_template.txt"))

;;; Batch
(defun jcs-insert-batch-template ()
  "Header format for batch file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/batch/batch_template.txt"))

;;; C
(defun jcs-insert-c-header-template ()
  "Header for C header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cc/c_header_template.txt"))

(defun jcs-insert-c-source-template ()
  "Header for C source file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cc/c_source_template.txt"))

;;; C++
(defun jcs-insert-c++-header-template ()
  "C++ Default Header Constrcutor and Destructor."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cc/c++_header_template.txt"))

(defun jcs-insert-c++-source-template ()
  "C++ Default Source Constrcutor and Destructor."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cc/c++_source_template.txt"))

;;; C#
(defun jcs-insert-cs-template ()
  "Header for CS header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cs/cs_template.txt"))

;;; CMake
(defun jcs-insert-cmake-template ()
  "CMake file format info."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cmake/cmake_template.txt"))

;;; COBOL
(defun jcs-insert-cobol-template ()
  "Template for COBOL."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/cobol/cobol_template.txt"))

;;; Elisp
(defun jcs-insert-elisp-template ()
  "Template for Elisp."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/elisp/elisp_template.txt"))

;;; Go
(defun jcs-insert-go-template ()
  "Header for Go header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/go/go_template.txt"))

;;; Java
(defun jcs-insert-java-template ()
  "Header for Java header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/java/java_template.txt"))

;;; JavaScript
(defun jcs-insert-js-template ()
  "Template for JavaScript."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/js/js_template.txt"))

;;; JayCeS
(defun jcs-insert-jayces-template ()
  "Header for JayCeS header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/jayces/jayces_template.txt"))

;;; Lua
(defun jcs-insert-lua-template ()
  "Lua file header format."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/lua/lua_template.txt"))

;;; Makefile
(defun jcs-makefile-format-info ()
  "File header format specific for makefile depends \
on language selected."
  (call-interactively 'jcs-ask-makefile-language))

(defun jcs-insert-makefile-cc-app-template ()
  "Default makefile template for normal application."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/makefile/makefile_cc_app.txt"))

(defun jcs-insert-makefile-cc-lib-template ()
  "Library makefile template for static library or shared library."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/makefile/makefile_cc_lib.txt"))

(defun jcs-insert-makefile-java-app-template ()
  "Template for makefile Java application."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/makefile/makefile_java_app.txt"))

(defun jcs-insert-makefile-java-lib-template ()
  "Template for makefile Java library."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/makefile/makefile_java_lib.txt"))

(defun jcs-insert-makefile-python-app-template ()
  "Template for makefile Python application."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/makefile/makefile_python_app.txt"))

(defun jcs-insert-makefile-python-lib-template ()
  "Template for makefile Python library."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/makefile_python_lib.txt"))

;;; HTML/CSS
(defun jcs-insert-html-template ()
  "Template for HTML."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/web/html_template.txt"))

(defun jcs-insert-css-template ()
  "Template for CSS."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/web/css_template.txt"))

;;; Perl
(defun jcs-insert-perl-template ()
  "Header for Perl header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/perl/perl_template.txt"))

;;; PHP
(defun jcs-insert-php-template ()
  "Template for PHP."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/web/php_template.txt"))

;;; Python
(defun jcs-insert-python-template ()
  "Header for Python header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/python/python_template.txt"))

;;; SASS
(defun jcs-insert-sass-template ()
  "Header for SASS header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/sass/sass_template.txt"))

;;; Scala
(defun jcs-insert-scala-template ()
  "Header for Scala header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/scala/scala_template.txt"))

;;; SCSS
(defun jcs-insert-scss-template ()
  "Header for SCSS header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/scss/scss_template.txt"))

;;; Shader
(defun jcs-insert-shader-template ()
  "Header for Shader header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/shader/shader_template.txt"))

;;; Shell
(defun jcs-insert-sh-template ()
  "Header for Shell header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/sh/sh_template.txt"))

;;; SQL
(defun jcs-insert-sql-template ()
  "Header for SQL header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/sql/sql_template.txt"))

;;; Text
(defun jcs-insert-txt-template ()
  "Header for Text header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/txt/txt_template.txt"))

;;; Vim script
(defun jcs-insert-vimscript-template ()
  "Header for XML header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/vimscript/vimscript_template.txt"))

;;; XML
(defun jcs-insert-xml-template ()
  "Header for XML header file."
  (jcs-insert-template-by-file-path "~/.emacs.d/elisp/jcs-ex/ex-template/xml/xml_template.txt"))

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-file-info-format.el file
