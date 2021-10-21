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
  (jcs-reload-file-info)
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

(defvar jcs-template--header-double-colon nil
  "Preload the double colon file info template.")

(defvar jcs-template--header-double-dash nil
  "Preload the double dash file info template.")

(defvar jcs-template--header-double-quote nil
  "Preload the double quote file info template.")

(defvar jcs-template--header-double-semicolon nil
  "Preload the double semicolon file info template.")

(defvar jcs-template--header-double-slash nil
  "Preload the double slash file info template.")

(defvar jcs-template--header-triple-slash nil
  "Preload the triple slash file info template.")

(defvar jcs-template--header-global nil
  "Preload the global file info template.")

(defvar jcs-template--header-sharp nil
  "Preload the sharp file info template.")

(defvar jcs-template--header-semicolon nil
  "Preload the semicolon file info template.")

(defvar jcs-template--header-single-quote nil
  "Preload the single quote file info template.")

(defvar jcs-template--header-tag nil
  "Preload the tag file info template.")

(defvar jcs-template--headers-loaded-p nil
  "Return non-nil, if headers are loaded as cache.")


(defun jcs-template-as-string (path)
  "Read template from PATH to string."
  (require 'f)
  (jcs-get-string-from-file (f-join jcs-template-dir path)))

(defun jcs-reload-file-info (&optional force)
  "Reload the header templates once.

If optional argument FORCE is non-nil, refresh cache once."
  (interactive)
  (when (or force (null jcs-template--headers-loaded-p))
    (setq jcs-template--header-double-colon (jcs-template-as-string "__header/d_colon.txt")
          jcs-template--header-double-dash (jcs-template-as-string "__header/d_dash.txt")
          jcs-template--header-double-quote (jcs-template-as-string "__header/d_quote.txt")
          jcs-template--header-double-semicolon (jcs-template-as-string "__header/d_semicolon.txt")
          jcs-template--header-double-slash (jcs-template-as-string "__header/d_slash.txt")
          jcs-template--header-triple-slash (jcs-template-as-string "__header/t_slash.txt")
          jcs-template--header-global (jcs-template-as-string "__header/global.txt")
          jcs-template--header-semicolon (jcs-template-as-string "__header/semicolon.txt")
          jcs-template--header-sharp (jcs-template-as-string "__header/sharp.txt")
          jcs-template--header-single-quote (jcs-template-as-string "__header/singlequote.txt")
          jcs-template--header-tag (jcs-template-as-string "__header/tag.txt")
          jcs-template--headers-loaded-p t)))

;;
;; (@* "Header" )
;;

(defun jcs-template-header-double-colon ()
  "Return the preloaded double colon file info template."
  (file-header-swap-keyword-template jcs-template--header-double-colon))

(defun jcs-template-header-double-dash ()
  "Return the preloaded double dash file info template."
  (file-header-swap-keyword-template jcs-template--header-double-dash))

(defun jcs-template-header-double-quote ()
  "Return the preloaded double quote file info template."
  (file-header-swap-keyword-template jcs-template--header-double-quote))

(defun jcs-template-header-double-semicolon ()
  "Return the preloaded double semicolon file info template."
  (file-header-swap-keyword-template jcs-template--header-double-semicolon))

(defun jcs-template-header-double-slash ()
  "Return the preloaded double slash file info template."
  (file-header-swap-keyword-template jcs-template--header-double-slash))

(defun jcs-template-header-triple-slash ()
  "Return the preloaded triple slash file info template."
  (file-header-swap-keyword-template jcs-template--header-triple-slash))

(defun jcs-template-header-global ()
  "Return the preloaded global file info template."
  (file-header-swap-keyword-template jcs-template--header-global))

(defun jcs-template-header-semicolon ()
  "Return the preloaded semicolon file info template."
  (file-header-swap-keyword-template jcs-template--header-semicolon))

(defun jcs-template-header-sharp ()
  "Return the preloaded sharp file info template."
  (file-header-swap-keyword-template jcs-template--header-sharp))

(defun jcs-template-header-single-quote ()
  "Return the preloaded single quote file info template."
  (file-header-swap-keyword-template jcs-template--header-single-quote))

(defun jcs-template-header-tag ()
  "Return the preloaded tag file info template."
  (file-header-swap-keyword-template jcs-template--header-tag))

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
