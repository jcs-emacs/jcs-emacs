;;; build.el --- Test the configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst jcs-build-test t
  "Define for build testing.")

;; Start regular Emacs file.
;;(load-file (expand-file-name "~/.emacs"))

(require 'package)

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
       package-enable-at-startup package-check-signature
       (pkgs '(0xc
    actionscript-mode
    adaptive-wrap
    alt-codes
    apache-mode
    applescript-mode
    atl-long-lines
    atl-markup
    auth-source-keytar
    auto-highlight-symbol
    auto-read-only
    auto-rename-tag
    basic-mode
    better-scroll
    browse-kill-ring
    buffer-move
    buffer-wrap
    cask
    cask-mode
    ccls
    centaur-tabs
    clojure-mode
    cmake-font-lock
    cmake-mode
    cobol-mode
    com-css-sort
    command-log-mode
    company-box
    company-c-headers
    company-emojify
    company-fuzzy
    counsel
    csharp-mode
    csproj-mode
    dap-mode
    dart-mode
    dashboard-ls
    define-it
    diff-hl
    diminish
    diminish-buffer
    dockerfile-mode
    docstr
    dumb-jump
    editorconfig
    el-mock
    elisp-def
    elisp-demos
    elixir-mode
    elm-mode
    emmet-mode
    emoji-github
    erlang
    ert-runner
    eshell-syntax-highlighting
    ess
    esup
    exec-path-from-shell
    expand-region
    feebleline
    ffmpeg-player
    fill-page
    flx
    flycheck-grammarly
    flycheck-languagetool
    flycheck-popup-tip
    flycheck-pos-tip
    fountain-mode
    fsharp-mode
    gdscript-mode
    gitattributes-mode
    gitconfig-mode
    github-browse-file
    gitignore-mode
    gitignore-templates
    glsl-mode
    go-mode
    google-this
    goto-char-preview
    goto-line-preview
    groovy-mode
    haskell-mode
    haxe-mode
    helpful
    highlight-escape-sequences
    highlight-indent-guides
    highlight-numbers
    hl-todo
    htmltagwrap
    ialign
    iedit
    impatient-mode
    impatient-showdown
    indent-control
    ini-mode
    isearch-project
    ivy-file-preview
    ivy-searcher
    javadoc-lookup
    jenkinsfile-mode
    js2-mode
    json-mode
    keypression
    kotlin-mode
    license-templates
    line-reminder
    logms
    logview
    lsp-dart
    lsp-docker
    lsp-grammarly
    lsp-haskell
    lsp-java
    lsp-latex
    lsp-ltex
    lsp-mssql
    lsp-origami
    lsp-pascal
    lsp-pyright
    lsp-sourcekit
    lsp-ui
    lua-mode
    manage-minor-mode-table
    markdown-toc
    masm-mode
    most-used-words
    move-text
    multiple-cursors
    nasm-mode
    nhexl-mode
    nix-mode
    org-bullets
    organize-imports-java
    package-build
    package-lint
    page-break-lines
    parse-it
    powerline
    powershell
    processing-mode
    project
    project-abbrev
    python-mode
    quelpa-leaf
    rainbow-mode
    region-occurrences-highlighter
    restart-emacs
    reveal-in-folder
    right-click-context
    rjsx-mode
    rust-mode
    scala-mode
    scrollable-quick-peek
    scss-mode
    shader-mode
    show-eol
    smex
    sql-indent
    ssass-mode
    swift-mode
    swiper
    togetherly
    transwin
    tree-sitter-indent
    tree-sitter-langs
    typescript-mode
    undercover
    undo-tree
    use-ttf
    vimrc-mode
    visual-regexp
    vs-dark-theme
    vs-light-theme
    vue-mode
    web-mode
    which-key
    yaml-mode
    yascroll
    yasnippet-snippets)))
  (package-initialize)

  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-refresh-contents) (package-install pkg)))
        pkgs)

  (add-hook 'kill-emacs-hook
            `(lambda ()
               (unless (boundp 'emacs-lsp-ci)
                 (delete-directory ,user-emacs-directory t)))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
