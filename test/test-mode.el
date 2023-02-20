;;; test-mode.el --- Test all major test-modes  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Test all major modes.
;;

;;; Code:

(load (concat user-emacs-directory "test/startup/test-startup.el"))

(defconst test-modes
  '(actionscript-mode
    ;;ada-mode
    ;;agda-mode
    apache-mode
    applescript-mode
    ;;nasm-mode masm-mode
    basic-mode
    bat-mode
    c-mode
    c++-mode
    clojure-mode
    cmake-mode
    cobol-mode
    coffee-mode
    crystal-mode
    csharp-mode
    csproj-mode
    css-mode
    d-mode
    dart-mode
    dist-file-mode
    dockerfile-mode
    elixir-mode
    elm-mode
    erlang-mode
    ;;feature-mode
    fountain-mode
    fsharp-mode
    gdscript-mode
    gitattributes-mode gitconfig-mode gitignore-mode
    go-mode
    groovy-mode
    haskell-mode
    haxe-mode
    ini-mode
    ;;java-mode
    javap-mode
    jayces-mode
    jenkinsfile-mode
    js-mode
    js-jsx-mode
    json-mode
    julia-mode
    less-css-mode
    lua-mode
    makefile-mode
    markdown-mode
    nginx-mode
    nix-mode
    objc-mode
    opascal-mode
    org-mode
    pascal-mode
    perl-mode
    phpt-mode
    powershell-mode
    conf-javaprop-mode
    protobuf-mode
    python-mode
    qml-mode
    r-mode
    robots-txt-mode
    ruby-mode
    rust-mode
    ssass-mode
    scala-mode
    scss-mode
    sh-mode
    glsl-mode hlsl-mode
    snippet-mode
    sql-mode
    swift-mode
    typescript-mode
    verilog-mode
    vimrc-mode
    vue-mode
    web-mode
    nxml-mode
    yaml-mode)
  "List of major-modes to test.")

(dolist (mode test-modes)
  (with-temp-buffer
    (message "Testing major-mode `%s'..." mode)
    (require mode nil t) (funcall mode)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; test-mode.el ends here
