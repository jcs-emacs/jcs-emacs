;;; jcs-template.el --- Template format  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst jcs-template-dir "~/.emacs.jcs/template/"
  "Template directory path for file headers.")

;;
;; (@* "File Header Insertion" )
;;

(defun jcs-insert-header-if-empty (insert-func &optional ci)
  "Execute INSERT-FUNC if empty, CI means `call-interactively'."
  (when (jcs-is-current-file-empty-p)
    (if ci (call-interactively insert-func) (funcall insert-func))
    (goto-char (point-min))))

(cl-defun jcs-insert-header-if-valid (reg-lst insert-func &key interactive success fail)
  "Insert the header if certain conditions met.

REG-LST is extension list represent by regular expression.
INSERT-FUNC is the function that will be use to call inserting header content.
INTERACTIVE is boolean check if called function interactively instead.
SUCCESS is callback after successfully inserted header content.
FAILED is callback if does NOT successfully inserted header content."
  (require 'f)
  (let (result)
    (when (and buffer-file-name
               (not (file-exists-p buffer-file-name))
               (jcs-contain-list-string-regexp reg-lst (f-filename buffer-file-name)))
      (setq result (jcs-insert-header-if-empty insert-func interactive)))
    (if result
        (when (functionp success) (funcall success))
      (when (functionp fail) (funcall fail)))
    result))

;;
;; (@* "Buffer String" )
;;

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

(defvar jcs--preload-triple-slash-file-info nil
  "Preload the triple slash file info template.")

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


(defun jcs-template-as-string (path)
  "Read template from PATH to string."
  (require 'f)
  (jcs-get-string-from-file (f-join jcs-template-dir path)))

(defun jcs-reload-file-info ()
  "Reload the template once.
If the template configuration file has change, this must be call
in order to take effect.  Half hot reloading process."
  (interactive)
  (setq jcs--preload-double-colon-file-info (jcs-template-as-string "__header/d_colon.txt")
        jcs--preload-double-dash-file-info (jcs-template-as-string "__header/d_dash.txt")
        jcs--preload-double-quote-file-info (jcs-template-as-string "__header/d_quote.txt")
        jcs--preload-double-semicolon-file-info (jcs-template-as-string "__header/d_semicolon.txt")
        jcs--preload-double-slash-file-info (jcs-template-as-string "__header/d_slash.txt")
        jcs--preload-triple-slash-file-info (jcs-template-as-string "__header/t_slash.txt")
        jcs--preload-global-file-info (jcs-template-as-string "__header/global.txt")
        jcs--preload-semicolon-file-info (jcs-template-as-string "__header/semicolon.txt")
        jcs--preload-sharp-file-info (jcs-template-as-string "__header/sharp.txt")
        jcs--preload-single-quote-file-info (jcs-template-as-string "__header/singlequote.txt")
        jcs--preload-tag-file-info (jcs-template-as-string "__header/tag.txt")))

;;
;; (@* "Header" )
;;

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

(defun jcs-get-triple-slash-file-info ()
  "Return the preloaded triple slash file info template."
  (file-header-swap-keyword-template jcs--preload-triple-slash-file-info))
(defun jcs-insert-triple-slash-file-info ()
  "Specific header format for triple slash."
  (insert (jcs-get-triple-slash-file-info)))

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

;;
;; (@* "Other Templates" )
;;

(defun jcs--file-header--insert (lang file)
  "Insert file header by language (LANG) and it's path (FILE)."
  (require 'f)
  (file-header-insert-template-by-file-path (f-join jcs-template-dir lang file)))

;;; ActionScript
(defun jcs-insert-actionscript-template ()
  "Template for ActionScript."
  (jcs--file-header--insert "actionscript" "default.txt"))

;;; Agda
(defun jcs-insert-agda-template ()
  "Template for Agda."
  (jcs--file-header--insert "agda" "default.txt"))

;;; AppleScript
(defun jcs-insert-applescript-template ()
  "Template for AppleScript."
  (jcs--file-header--insert "applescript" "default.txt"))

;; Assembly Language
(defun jcs-insert-masm-template ()
  "Header for MASM file."
  (jcs--file-header--insert "assembly" "masm.txt"))

(defun jcs-insert-nasm-template ()
  "Header for NASM file."
  (jcs--file-header--insert "assembly" "nasm.txt"))

;;; BASIC
(defun jcs-insert-basic-template ()
  "Header format for BASIC file."
  (jcs--file-header--insert "basic" "default.txt"))

;;; Batch
(defun jcs-insert-batch-template ()
  "Header format for batch file."
  (jcs--file-header--insert "batch" "default.txt"))

;;; C
(defun jcs-insert-c-header-template ()
  "Header for C header file."
  (jcs--file-header--insert "c" "header.txt"))

(defun jcs-insert-c-source-template ()
  "Header for C source file."
  (jcs--file-header--insert "c" "source.txt"))

;;; C++
(defun jcs-insert-c++-header-template ()
  "Header for C++ header file."
  (jcs--file-header--insert "c++" "header.txt"))

(defun jcs-insert-c++-source-template ()
  "Header for C++ source file."
  (jcs--file-header--insert "c++" "source.txt"))

(defun jcs-insert-c++-unreal-header-template--actor ()
  "Header for Unreal C++ header file with actor type."
  (jcs--file-header--insert "c++" "unreal/actor/header.txt"))

(defun jcs-insert-c++-unreal-source-template--actor ()
  "Header for Unreal C++ source file with actor type."
  (jcs--file-header--insert "c++" "unreal/actor/source.txt"))

(defun jcs-insert-c++-unreal-header-template--actor-component ()
  "Header for Unreal C++ header file with other type."
  (jcs--file-header--insert "c++" "unreal/actor-component/header.txt"))

(defun jcs-insert-c++-unreal-source-template--actor-component ()
  "Header for Unreal C++ source file with other type."
  (jcs--file-header--insert "c++" "unreal/actor-component/source.txt"))

;;; C#
(defun jcs-insert-csharp-template ()
  "Header for C# header file."
  (jcs--file-header--insert "csharp" "default.txt"))

(defun jcs-insert-csharp-unity-template ()
  "Header for Unity C# header file."
  (jcs--file-header--insert "csharp" "unity.txt"))

;;; Clojure
(defun jcs-insert-clojure-template ()
  "Header for Clojure header file."
  (jcs--file-header--insert "clojure" "default.txt"))

;;; CMake
(defun jcs-insert-cmake-template ()
  "CMake file format info."
  (jcs--file-header--insert "cmake" "default.txt"))

;;; COBOL
(defun jcs-insert-cobol-template ()
  "Template for COBOL."
  (jcs--file-header--insert "cobol" "default.txt"))

;;; CSS
(defun jcs-insert-css-template ()
  "Template for CSS."
  (jcs--file-header--insert "css" "default.txt"))

;;; Dart
(defun jcs-insert-dart-template ()
  "Template for Dart."
  (jcs--file-header--insert "dart" "default.txt"))

;;; Elixir
(defun jcs-insert-elixir-template ()
  "Template for Elixir."
  (jcs--file-header--insert "elixir" "default.txt"))

;;; Elm
(defun jcs-insert-elm-template ()
  "Template for Elm."
  (jcs--file-header--insert "elm" "default.txt"))

;;; Emacs Lisp
(defun jcs-insert-emacs-lisp-template ()
  "Template for Emacs Lisp."
  (jcs--file-header--insert "elisp" "default.txt"))

;;; Erlang
(defun jcs-insert-erlang-template ()
  "Template for Erlang Lisp."
  (jcs--file-header--insert "erlang" "default.txt"))

;;; Fountain
(defun jcs-insert-fountain-template ()
  "Template for Fountain Lisp."
  (jcs--file-header--insert "fountain" "default.txt"))

;;; F#
(defun jcs-insert-fsharp-template ()
  "Header for F# header file."
  (jcs--file-header--insert "fsharp" "default.txt"))

;;; GLSL
(defun jcs-insert-glsl-template ()
  "Header for GLSL header file."
  (jcs--file-header--insert "shader" "default_glsl.txt"))

;;; Go
(defun jcs-insert-go-template ()
  "Header for Go header file."
  (jcs--file-header--insert "go" "default.txt"))

;;; Godot Script
(defun jcs-insert-gdscript-template ()
  "Header for Godot Script header file."
  (jcs--file-header--insert "gdscript" "default.txt"))

;;; Groovy
(defun jcs-insert-groovy-template ()
  "Header for Groovy header file."
  (jcs--file-header--insert "groovy" "default.txt"))

;;; Haskell
(defun jcs-insert-haskell-template ()
  "Template for Haskell."
  (jcs--file-header--insert "haskell" "default.txt"))

;;; Haxe
(defun jcs-insert-haxe-template ()
  "Template for Haxe."
  (jcs--file-header--insert "haxe" "default.txt"))

;;; HTML
(defun jcs-insert-html-template ()
  "Template for HTML."
  (jcs--file-header--insert "web" "default_html.txt"))

;;; Java
(defun jcs-insert-java-template ()
  "Header for Java header file."
  (jcs--file-header--insert "java" "default.txt"))

;;; JavaScript
(defun jcs-insert-js-template ()
  "Template for JavaScript."
  (jcs--file-header--insert "js" "default.txt"))

;;; JavaScript XML
(defun jcs-insert-jsx-template ()
  "Template for JavaScript XML (JSX)."
  (jcs--file-header--insert "jsx" "default.txt"))

(defun jcs-insert-jsx-react-js-template ()
  "Template for React JS JavaScript XML (JSX)."
  (jcs--file-header--insert "jsx" "react/js.txt"))

(defun jcs-insert-jsx-react-native-template ()
  "Template for React Native JavaScript XML (JSX)."
  (jcs--file-header--insert "jsx" "react/native.txt"))

;;; JayCeS
(defun jcs-insert-jayces-template ()
  "Header for JayCeS header file."
  (jcs--file-header--insert "jayces" "default.txt"))

;;; Jenkins
(defun jcs-insert-jenkinsfile-template ()
  "Header for Jenkinsfile."
  (jcs--file-header--insert "jenkins" "default.txt"))

;;; Kotlin
(defun jcs-insert-kotlin-template ()
  "Header for Kotlin header file."
  (jcs--file-header--insert "kotlin" "default.txt"))

;;; LESS
(defun jcs-insert-less-template ()
  "Header for LESS header file."
  (jcs--file-header--insert "less" "default.txt"))

;;; Lisp
(defun jcs-insert-lisp-template ()
  "Lisp file header format."
  (jcs--file-header--insert "lisp" "default.txt"))

;;; Lua
(defun jcs-insert-lua-template ()
  "Lua file header format."
  (jcs--file-header--insert "lua" "default.txt"))

;;; Makefile
(defun jcs-makefile-format-info ()
  "File header format specific for makefile depends on language selected."
  (call-interactively 'jcs-ask-makefile-language))

(defun jcs-insert-makefile-cc-app-template ()
  "Default makefile template for normal application."
  (jcs--file-header--insert "makefile" "cc/app.txt"))

(defun jcs-insert-makefile-cc-lib-template ()
  "Library makefile template for static library or shared library."
  (jcs--file-header--insert "makefile" "cc/lib.txt"))

(defun jcs-insert-makefile-java-app-template ()
  "Template for makefile Java application."
  (jcs--file-header--insert "makefile" "java/app.txt"))

(defun jcs-insert-makefile-java-lib-template ()
  "Template for makefile Java library."
  (jcs--file-header--insert "makefile" "java/lib.txt"))

(defun jcs-insert-makefile-python-app-template ()
  "Template for makefile Python application."
  (jcs--file-header--insert "makefile" "python/app.txt"))

(defun jcs-insert-makefile-python-lib-template ()
  "Template for makefile Python library."
  (jcs--file-header--insert "makefile" "python/lib.txt"))

;;; Markdown
(defun jcs-insert-markdown-template ()
  "Header for Markdown header file."
  (jcs--file-header--insert "markdown" "default.txt"))

;;; Nix
(defun jcs-insert-nix-template ()
  "Header for Nix header file."
  (jcs--file-header--insert "nix" "default.txt"))

;;; Object Pascal (Delphi)
(defun jcs-insert-opascal-template ()
  "Header for Object Pascal header file."
  (jcs--file-header--insert "opascal" "default.txt"))

;;; Objective-C
(defun jcs-insert-objc-header-template ()
  "Header for Objective-C header file."
  (jcs--file-header--insert "objc" "header.txt"))

(defun jcs-insert-objc-source-template ()
  "Header for Objective-C source file."
  (jcs--file-header--insert "objc" "source.txt"))

;;; Pascal
(defun jcs-insert-pascal-template ()
  "Header for Pascal header file."
  (jcs--file-header--insert "pascal" "default.txt"))

;;; Perl
(defun jcs-insert-perl-template ()
  "Header for Perl header file."
  (jcs--file-header--insert "perl" "default.txt"))

;;; PHP
(defun jcs-insert-php-template ()
  "Template for PHP."
  (jcs--file-header--insert "web" "default_php.txt"))

;;; PowerShell
(defun jcs-insert-powershell-template ()
  "Header for PowerShell header file."
  (jcs--file-header--insert "powershell" "default.txt"))

;;; Processing
(defun jcs-insert-processing-template ()
  "Header for Processing file."
  (jcs--file-header--insert "processing" "default.txt"))

;;; Python
(defun jcs-insert-python-template ()
  "Python template."
  (jcs--file-header--insert "python" "default.txt"))

(defun jcs-insert-python-class-template ()
  "Python class template."
  (jcs--file-header--insert "python" "class.txt"))

;;; R
(defun jcs-insert-r-template ()
  "Header for R header file."
  (jcs--file-header--insert "r" "default.txt"))

;;; Ruby
(defun jcs-insert-ruby-template ()
  "Header for Ruby header file."
  (jcs--file-header--insert "ruby" "default.txt"))

;;; Rust
(defun jcs-insert-rust-template ()
  "Header for Rust header file."
  (jcs--file-header--insert "rust" "default.txt"))

;;; Sass
(defun jcs-insert-sass-template ()
  "Header for SASS header file."
  (jcs--file-header--insert "sass" "default.txt"))

;;; Scala
(defun jcs-insert-scala-template ()
  "Header for Scala header file."
  (jcs--file-header--insert "scala" "default.txt"))

;;; SCSS
(defun jcs-insert-scss-template ()
  "Header for SCSS header file."
  (jcs--file-header--insert "scss" "default.txt"))

;;; Shader
(defun jcs-insert-shader-template ()
  "Header for Shader header file."
  (jcs--file-header--insert "shader" "default_shader.txt"))

;;; Shell
(defun jcs-insert-sh-template ()
  "Header for Shell header file."
  (jcs--file-header--insert "sh" "default.txt"))

;;; SQL
(defun jcs-insert-sql-template ()
  "Header for SQL header file."
  (jcs--file-header--insert "sql" "default.txt"))

;;; Swift
(defun jcs-insert-swift-template ()
  "Header for Swift header file."
  (jcs--file-header--insert "swift" "default.txt"))

;;; Text
(defun jcs-insert-text-template ()
  "Header for Text header file."
  (jcs--file-header--insert "text" "default.txt"))

;;; TypeScript
(defun jcs-insert-typescript-template ()
  "Header for TypeScript header file."
  (jcs--file-header--insert "typescript" "default.txt"))

(defun jcs-insert-typescript-cocos-creator-template ()
  "Header for Cocos Creator TypeScript header file."
  (jcs--file-header--insert "typescript" "cocos_creator.txt"))

;;; Verilog
(defun jcs-insert-verilog-template ()
  "Header for Verilog header file."
  (jcs--file-header--insert "verilog" "default.txt"))

;;; Vim script
(defun jcs-insert-vimscript-template ()
  "Header for Vimscript header file."
  (jcs--file-header--insert "vimscript" "default.txt"))

;;; Vue
(defun jcs-insert-vue-template ()
  "Header for Vue header file."
  (jcs--file-header--insert "vue" "default.txt"))

;;; XML
(defun jcs-insert-xml-template ()
  "Header for XML header file."
  (jcs--file-header--insert "xml" "default.txt"))

;;; YAML
(defun jcs-insert-yaml-template ()
  "Header for YAML header file."
  (jcs--file-header--insert "yaml" "default.txt"))

(provide 'jcs-template)
;;; jcs-template.el ends here
