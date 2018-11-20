;; ========================================================================
;; $File: jcs-file-info-format.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;========================================
;;      JENCHIEH KEY GLOBAL INFO
;;----------------------------------

(defvar jcs-template-config-filepath "~/.emacs.jcs/template/template_config.properties"
  "File path to template config properties.")

(defun jcs-swap-keyword-template (template-str)
  "Swap all keyword in template to proper information.
TEMPLATE-STR : template string data."
  (let ((tmp-ini-list '())
        (tmp-keyword "")
        (tmp-value "")
        (tmp-index 0))

    ;; parse and get the list of keyword and value.
    (setq tmp-ini-list (jcs-parse-ini jcs-template-config-filepath))

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
      ;; Add 2 to skip keyword and value at the same time.
      (setq tmp-index (+ tmp-index 2))))

  ;; return itself.
  template-str)

(defun jcs-get-template-by-file-path (filePath)
  "Replace keyword and return modefieded template string.
FILEPATH : template file."
  (jcs-get-template-by-file-path (jcs-get-string-from-file filePath)))

(defun jcs-insert-template-by-file-path (filePath)
  "Swap all keywords then insert it to current buffer.
FILEPATH : file path to insert and swap keyword."
  (let ((template-str (jcs-get-string-from-file filePath)))
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

(defvar jcs-preload-single-quote-file-info nil
  "Preload the single quote file info template.")

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
  (setq jcs-preload-global-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/header/global_template.txt"))
  (setq jcs-preload-tag-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/header/tag_template.txt"))
  (setq jcs-preload-manage-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/header/manage_template.txt"))
  (setq jcs-preload-semi-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/header/semi_template.txt"))
  (setq jcs-preload-single-quote-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/header/singlequote_template.txt"))
  (setq jcs-preload-double-quote-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/header/doublequote_template.txt"))
  (setq jcs-preload-double-colon-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/header/doublecolon_template.txt"))
  (setq jcs-preload-double-dash-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/header/doubledash_template.txt")))

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

(defun jcs-get-single-quote-file-info ()
  "Return the preloaded single quote file info template."
  (jcs-swap-keyword-template jcs-preload-single-quote-file-info))

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
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/header/global_template.txt"))

(defun jcs-insert-tag-file-info ()
  "Tag file header info for tag language."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/header/tag_template.txt"))

(defun jcs-insert-manage-file-info ()
  "Any managing file format.
Text file, batch file, shell script, etc."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/header/manage_template.txt"))

(defun jcs-insert-semi-file-info ()
  "Specific header format for Assembly Language/lisp/elisp, etc."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/header/semi_template.txt"))

(defun jcs-insert-single-quote-file-info ()
  "Specific header format for single qoute."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/header/singlequote_template.txt"))

(defun jcs-insert-double-quote-file-info ()
  "Specific header format for double quote."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/header/doublequote_template.txt"))

(defun jcs-insert-double-colon-file-info ()
  "Specific header format for double semi-colon."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/header/doublecolon_template.txt"))


;;---------------------------------------------
;; Other Template
;;---------------------------------------------

;;; ActionScript
(defun jcs-insert-actionscript-template ()
  "Template for ActionScript."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/actionscript/actionscript_template.txt"))

;; Assembly Language
(defun jcs-insert-asm-template ()
  "Header for Assembly Language file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/assembly/assembly_template.txt"))

;;; BASIC
(defun jcs-insert-basic-template ()
  "Header format for BASIC file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/basic/basic_template.txt"))

;;; Batch
(defun jcs-insert-batch-template ()
  "Header format for batch file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/batch/batch_template.txt"))

;;; C
(defun jcs-insert-c-header-template ()
  "Header for C header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/cc/c_header_template.txt"))

(defun jcs-insert-c-source-template ()
  "Header for C source file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/cc/c_source_template.txt"))

;;; C++
(defun jcs-insert-c++-header-template ()
  "C++ Default Header Constrcutor and Destructor."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/cc/c++_header_template.txt"))

(defun jcs-insert-c++-source-template ()
  "C++ Default Source Constrcutor and Destructor."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/cc/c++_source_template.txt"))

;;; C#
(defun jcs-insert-cs-template ()
  "Header for CS header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/csharp/csharp_template.txt"))

;;; Clojure
(defun jcs-insert-clojure-template ()
  "Header for Clojure header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/clojure/clj_template.txt"))

;;; CMake
(defun jcs-insert-cmake-template ()
  "CMake file format info."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/cmake/cmake_template.txt"))

;;; COBOL
(defun jcs-insert-cobol-template ()
  "Template for COBOL."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/cobol/cobol_template.txt"))

;;; CSS
(defun jcs-insert-css-template ()
  "Template for CSS."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/web/css_template.txt"))

;;; Elisp
(defun jcs-insert-elisp-template ()
  "Template for Elisp."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/elisp/elisp_template.txt"))

;;; Go
(defun jcs-insert-go-template ()
  "Header for Go header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/go/go_template.txt"))

;;; Haskell
(defun jcs-insert-haskell-template ()
  "Template for Haskell."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/haskell/haskell_template.txt"))

;;; Haxe
(defun jcs-insert-haxe-template ()
  "Template for Haxe."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/haxe/haxe_template.txt"))

;;; HTML
(defun jcs-insert-html-template ()
  "Template for HTML."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/web/html_template.txt"))

;;; Java
(defun jcs-insert-java-template ()
  "Header for Java header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/java/java_template.txt"))

;;; JavaScript
(defun jcs-insert-js-template ()
  "Template for JavaScript."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/javascript/javascript_template.txt"))

;;; JayCeS
(defun jcs-insert-jayces-template ()
  "Header for JayCeS header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/jayces/jayces_template.txt"))

;;; Lisp
(defun jcs-insert-lisp-template ()
  "Lisp file header format."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/lisp/lisp_template.txt"))

;;; Lua
(defun jcs-insert-lua-template ()
  "Lua file header format."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/lua/lua_template.txt"))

;;; Makefile
(defun jcs-makefile-format-info ()
  "File header format specific for makefile depends \
on language selected."
  (call-interactively 'jcs-ask-makefile-language))

(defun jcs-insert-makefile-cc-app-template ()
  "Default makefile template for normal application."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/makefile/makefile_cc_app.txt"))

(defun jcs-insert-makefile-cc-lib-template ()
  "Library makefile template for static library or shared library."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/makefile/makefile_cc_lib.txt"))

(defun jcs-insert-makefile-java-app-template ()
  "Template for makefile Java application."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/makefile/makefile_java_app.txt"))

(defun jcs-insert-makefile-java-lib-template ()
  "Template for makefile Java library."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/makefile/makefile_java_lib.txt"))

(defun jcs-insert-makefile-python-app-template ()
  "Template for makefile Python application."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/makefile/makefile_python_app.txt"))

(defun jcs-insert-makefile-python-lib-template ()
  "Template for makefile Python library."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/makefile_python_lib.txt"))

;;; Perl
(defun jcs-insert-perl-template ()
  "Header for Perl header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/perl/perl_template.txt"))

;;; PHP
(defun jcs-insert-php-template ()
  "Template for PHP."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/web/php_template.txt"))

;;; Processing
(defun jcs-insert-processing-template ()
  "Header for Processing file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/processing/processing_template.txt"))

;;; Python
(defun jcs-insert-python-template ()
  "Header for Python header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/python/python_template.txt"))

;;; Ruby
(defun jcs-insert-ruby-template ()
  "Header for Ruby header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/ruby/ruby_template.txt"))

;;; Rust
(defun jcs-insert-rust-template ()
  "Header for Rust header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/rust/rust_template.txt"))

;;; SASS
(defun jcs-insert-sass-template ()
  "Header for SASS header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/sass/sass_template.txt"))

;;; Scala
(defun jcs-insert-scala-template ()
  "Header for Scala header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/scala/scala_template.txt"))

;;; SCSS
(defun jcs-insert-scss-template ()
  "Header for SCSS header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/scss/scss_template.txt"))

;;; Shader
(defun jcs-insert-shader-template ()
  "Header for Shader header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/shader/shader_template.txt"))

;;; Shell
(defun jcs-insert-sh-template ()
  "Header for Shell header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/sh/sh_template.txt"))

;;; SQL
(defun jcs-insert-sql-template ()
  "Header for SQL header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/sql/sql_template.txt"))

;;; Swift
(defun jcs-insert-swift-template ()
  "Header for Swift header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/swift/swift_template.txt"))

;;; Text
(defun jcs-insert-txt-template ()
  "Header for Text header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/txt/txt_template.txt"))

;;; TypeScript
(defun jcs-insert-typescript-template ()
  "Header for TypeScript header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/typescript/typescript_template.txt"))

;;; Verilog
(defun jcs-insert-verilog-template ()
  "Header for Verilog header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/verilog/verilog_template.txt"))

;;; Vim script
(defun jcs-insert-vimscript-template ()
  "Header for Vimscript header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/vimscript/vimscript_template.txt"))

;;; XML
(defun jcs-insert-xml-template ()
  "Header for XML header file."
  (jcs-insert-template-by-file-path "~/.emacs.jcs/template/xml/xml_template.txt"))
