;;; jcs-template.el --- Template format.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; File Header Insertion

(defun jcs-insert-header-if-valid (reg-lst insert-func &optional ci)
  "Insert the header if certain conditions met.
If one of the REG-LST, we execute INSERT-FUNC then, CI means `call-interactively'."
  (require 'f)
  (if (and buffer-file-name
           (not (file-exists-p buffer-file-name))
           (jcs-is-contain-list-string-regexp reg-lst (f-filename buffer-file-name)))
      (jcs-insert-header-if-empty insert-func ci)
    nil))

(defun jcs-insert-header-if-empty (insert-func &optional ci)
  "Execute INSERT-FUNC if empty, CI means `call-interactively'."
  (if (jcs-is-current-file-empty-p)
      (progn
        (if ci (call-interactively insert-func) (funcall insert-func))
        (goto-char (point-min)))
    nil))

;;----------------------------------------------------------------------------
;; Buffer String

(defvar jcs--preload-double-colon-file-info nil
  "Preload the double colon file info template.")

(defvar jcs--preload-double-dash-file-info nil
  "Preload the double dash file info template.")

(defvar jcs--preload-double-quote-file-info nil
  "Preload the double quote file info template.")

(defvar jcs--preload-double-semicolon-file-info nil
  "Preload the double semicolon file info template.")

(defvar jcs--preload-double-slash-file-info nil
  "Preload the double slash file info template.")

(defvar jcs--preload-global-file-info nil
  "Preload the global file info template.")

(defvar jcs--preload-sharp-file-info nil
  "Preload the sharp file info template.")

(defvar jcs--preload-semicolon-file-info nil
  "Preload the semicolon file info template.")

(defvar jcs--preload-single-quote-file-info nil
  "Preload the single quote file info template.")

(defvar jcs--preload-tag-file-info nil
  "Preload the tag file info template.")


;;;###autoload
(defun jcs-reload-file-info ()
  "Reload the template once.
If the template configuration file has change, this must be call
in order to take effect.  Half hot reloading process."
  (interactive)
  (setq jcs--preload-double-colon-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/d_colon.txt"))
  (setq jcs--preload-double-dash-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/d_dash.txt"))
  (setq jcs--preload-double-quote-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/d_quote.txt"))
  (setq jcs--preload-double-semicolon-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/d_semicolon.txt"))
  (setq jcs--preload-double-slash-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/d_slash.txt"))
  (setq jcs--preload-global-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/global.txt"))
  (setq jcs--preload-semicolon-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/semicolon.txt"))
  (setq jcs--preload-sharp-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/sharp.txt"))
  (setq jcs--preload-single-quote-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/singlequote.txt"))
  (setq jcs--preload-tag-file-info (jcs-get-string-from-file "~/.emacs.jcs/template/__header/tag.txt")))

;;----------------------------------------------------------------------------
;; Header

(defun jcs-get-double-colon-file-info ()
  "Return the preloaded double colon file info template."
  (file-header-swap-keyword-template jcs--preload-double-colon-file-info))
(defun jcs-insert-double-colon-file-info ()
  "Specific header format for double semi-colon."
  (insert (jcs-get-double-colon-file-info)))

(defun jcs-get-double-dash-file-info ()
  "Return the preloaded double dash file info template."
  (file-header-swap-keyword-template jcs--preload-double-dash-file-info))
(defun jcs-insert-double-dash-file-info ()
  "Specific header format for double dash."
  (insert (jcs-get-double-dash-file-info)))

(defun jcs-get-double-quote-file-info ()
  "Return the preloaded double quote file info template."
  (file-header-swap-keyword-template jcs--preload-double-quote-file-info))
(defun jcs-insert-double-quote-file-info ()
  "Specific header format for double quote."
  (insert (jcs-get-double-quote-file-info)))

(defun jcs-get-double-semicolon-file-info ()
  "Return the preloaded double semicolon file info template."
  (file-header-swap-keyword-template jcs--preload-double-semicolon-file-info))
(defun jcs-insert-double-semicolon-file-info ()
  "Specific header format for double semicolon."
  (insert (jcs-get-double-semicolon-file-info)))

(defun jcs-get-double-slash-file-info ()
  "Return the preloaded double slash file info template."
  (file-header-swap-keyword-template jcs--preload-double-slash-file-info))
(defun jcs-insert-double-slash-file-info ()
  "Specific header format for double slash."
  (insert (jcs-get-double-slash-file-info)))

(defun jcs-get-global-file-info ()
  "Return the preloaded global file info template."
  (file-header-swap-keyword-template jcs--preload-global-file-info))
(defun jcs-insert-global-file-info ()
  "Using '/*' '*/' for commenting programming languages."
  (insert (jcs-get-global-file-info)))

(defun jcs-get-semicolon-file-info ()
  "Return the preloaded semicolon file info template."
  (file-header-swap-keyword-template jcs--preload-semicolon-file-info))
(defun jcs-insert-semicolon-file-info ()
  "Specific header format for semicolon."
  (insert (jcs-get-semicolon-file-info)))

(defun jcs-get-sharp-file-info ()
  "Return the preloaded sharp file info template."
  (file-header-swap-keyword-template jcs--preload-sharp-file-info))
(defun jcs-insert-sharp-file-info ()
  "Specific header format for sharp."
  (insert (jcs-get-sharp-file-info)))

(defun jcs-get-single-quote-file-info ()
  "Return the preloaded single quote file info template."
  (file-header-swap-keyword-template jcs--preload-single-quote-file-info))
(defun jcs-insert-single-quote-file-info ()
  "Specific header format for single qoute."
  (insert (jcs-get-single-quote-file-info)))

(defun jcs-get-tag-file-info ()
  "Return the preloaded tag file info template."
  (file-header-swap-keyword-template jcs--preload-tag-file-info))
(defun jcs-insert-tag-file-info ()
  "Tag file header info for tag language."
  (insert (jcs-get-tag-file-info)))

;;----------------------------------------------------------------------------
;; Other Template

;;; ActionScript
(defun jcs-insert-actionscript-template ()
  "Template for ActionScript."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/actionscript/default.txt"))

;; Assembly Language
(defun jcs-insert-masm-template ()
  "Header for MASM file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/assembly/masm.txt"))

(defun jcs-insert-nasm-template ()
  "Header for NASM file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/assembly/nasm.txt"))

;;; BASIC
(defun jcs-insert-basic-template ()
  "Header format for BASIC file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/basic/default.txt"))

;;; Batch
(defun jcs-insert-batch-template ()
  "Header format for batch file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/batch/default.txt"))

;;; C
(defun jcs-insert-c-header-template ()
  "Header for C header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/c/header.txt"))

(defun jcs-insert-c-source-template ()
  "Header for C source file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/c/source.txt"))

;;; C++
(defun jcs-insert-c++-header-template ()
  "C++ Default Header Constrcutor and Destructor."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/c++/header.txt"))

(defun jcs-insert-c++-source-template ()
  "C++ Default Source Constrcutor and Destructor."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/c++/source.txt"))

;;; C#
(defun jcs-insert-csharp-template ()
  "Header for CS header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/csharp/default.txt"))

(defun jcs-insert-csharp-unity-template ()
  "Header for Unity CS header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/csharp/unity.txt"))

;;; Clojure
(defun jcs-insert-clojure-template ()
  "Header for Clojure header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/clojure/default.txt"))

;;; CMake
(defun jcs-insert-cmake-template ()
  "CMake file format info."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/cmake/default.txt"))

;;; COBOL
(defun jcs-insert-cobol-template ()
  "Template for COBOL."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/cobol/default.txt"))

;;; CSS
(defun jcs-insert-css-template ()
  "Template for CSS."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/css/default.txt"))

;;; Dart
(defun jcs-insert-dart-template ()
  "Template for Dart."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/dart/default.txt"))

;;; Elixir
(defun jcs-insert-elixir-template ()
  "Template for Elixir."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/elixir/default.txt"))

;;; Emacs Lisp
(defun jcs-insert-emacs-lisp-template ()
  "Template for Emacs Lisp."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/elisp/default.txt"))

;;; Erlang
(defun jcs-insert-erlang-template ()
  "Template for Erlang Lisp."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/erlang/default.txt"))

;;; GLSL
(defun jcs-insert-glsl-template ()
  "Header for GLSL header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/shader/default_glsl.txt"))

;;; Go
(defun jcs-insert-go-template ()
  "Header for Go header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/go/default.txt"))

;;; Godot Script
(defun jcs-insert-gdscript-template ()
  "Header for Godot Script header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/gdscript/default.txt"))

;;; Haskell
(defun jcs-insert-haskell-template ()
  "Template for Haskell."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/haskell/default.txt"))

;;; Haxe
(defun jcs-insert-haxe-template ()
  "Template for Haxe."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/haxe/default.txt"))

;;; HTML
(defun jcs-insert-html-template ()
  "Template for HTML."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/web/default_html.txt"))

;;; Java
(defun jcs-insert-java-template ()
  "Header for Java header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/java/default.txt"))

;;; JavaScript
(defun jcs-insert-js-template ()
  "Template for JavaScript."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/js/default.txt"))

;;; JavaScript XML
(defun jcs-insert-jsx-template ()
  "Template for JavaScript XML (JSX)."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/jsx/default.txt"))

;;; JayCeS
(defun jcs-insert-jayces-template ()
  "Header for JayCeS header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/jayces/default.txt"))

;;; Kotlin
(defun jcs-insert-kotlin-template ()
  "Header for Kotlin header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/kotlin/default.txt"))

;;; LESS
(defun jcs-insert-less-template ()
  "Header for LESS header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/less/default.txt"))

;;; Lisp
(defun jcs-insert-lisp-template ()
  "Lisp file header format."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/lisp/default.txt"))

;;; Lua
(defun jcs-insert-lua-template ()
  "Lua file header format."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/lua/default.txt"))

;;; Makefile
(defun jcs-makefile-format-info ()
  "File header format specific for makefile depends on language selected."
  (call-interactively 'jcs-ask-makefile-language))

(defun jcs-insert-makefile-cc-app-template ()
  "Default makefile template for normal application."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/makefile/cc_app.txt"))

(defun jcs-insert-makefile-cc-lib-template ()
  "Library makefile template for static library or shared library."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/makefile/cc_lib.txt"))

(defun jcs-insert-makefile-java-app-template ()
  "Template for makefile Java application."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/makefile/java_app.txt"))

(defun jcs-insert-makefile-java-lib-template ()
  "Template for makefile Java library."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/makefile/java_lib.txt"))

(defun jcs-insert-makefile-python-app-template ()
  "Template for makefile Python application."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/makefile/python_app.txt"))

(defun jcs-insert-makefile-python-lib-template ()
  "Template for makefile Python library."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/makefile/python_lib.txt"))

;;; Object Pascal (Delphi)
(defun jcs-insert-opascal-template ()
  "Header for Object Pascal header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/opascal/default.txt"))

;;; Objective-C
(defun jcs-insert-objc-header-template ()
  "Header for Objective-C header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/objc/header.txt"))

(defun jcs-insert-objc-source-template ()
  "Header for Objective-C source file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/objc/source.txt"))

;;; Pascal
(defun jcs-insert-pascal-template ()
  "Header for Pascal header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/pascal/default.txt"))

;;; Perl
(defun jcs-insert-perl-template ()
  "Header for Perl header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/perl/default.txt"))

;;; PHP
(defun jcs-insert-php-template ()
  "Template for PHP."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/web/default_php.txt"))

;;; Processing
(defun jcs-insert-processing-template ()
  "Header for Processing file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/processing/default.txt"))

;;; Python
(defun jcs-insert-python-template ()
  "Python template."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/python/default.txt"))

(defun jcs-insert-python-class-template ()
  "Python class template."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/python/class.txt"))

;;; Ruby
(defun jcs-insert-ruby-template ()
  "Header for Ruby header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/ruby/default.txt"))

;;; Rust
(defun jcs-insert-rust-template ()
  "Header for Rust header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/rust/default.txt"))

;;; Sass
(defun jcs-insert-sass-template ()
  "Header for SASS header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/sass/default.txt"))

;;; Scala
(defun jcs-insert-scala-template ()
  "Header for Scala header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/scala/default.txt"))

;;; SCSS
(defun jcs-insert-scss-template ()
  "Header for SCSS header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/scss/default.txt"))

;;; Shader
(defun jcs-insert-shader-template ()
  "Header for Shader header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/shader/default_shader.txt"))

;;; Shell
(defun jcs-insert-sh-template ()
  "Header for Shell header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/sh/default.txt"))

;;; SQL
(defun jcs-insert-sql-template ()
  "Header for SQL header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/sql/default.txt"))

;;; Swift
(defun jcs-insert-swift-template ()
  "Header for Swift header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/swift/default.txt"))

;;; Text
(defun jcs-insert-text-template ()
  "Header for Text header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/text/default.txt"))

;;; TypeScript
(defun jcs-insert-typescript-template ()
  "Header for TypeScript header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/typescript/default.txt"))

(defun jcs-insert-typescript-cocos-creator-template ()
  "Header for Cocos Creator TypeScript header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/typescript/cocos_creator.txt"))

;;; Verilog
(defun jcs-insert-verilog-template ()
  "Header for Verilog header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/verilog/default.txt"))

;;; Vim script
(defun jcs-insert-vimscript-template ()
  "Header for Vimscript header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/vimscript/default.txt"))

;;; Vue
(defun jcs-insert-vue-template ()
  "Header for Vue header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/vue/default.txt"))

;;; XML
(defun jcs-insert-xml-template ()
  "Header for XML header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/xml/default.txt"))

;;; YAML
(defun jcs-insert-yaml-template ()
  "Header for YAML header file."
  (file-header-insert-template-by-file-path "~/.emacs.jcs/template/yaml/default.txt"))

(provide 'jcs-template)
;;; jcs-template.el ends here
