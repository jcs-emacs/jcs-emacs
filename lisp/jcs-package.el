;;; jcs-package.el --- Package archive related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq package-archives
      '(("gnu"      . "http://elpa.gnu.org/packages/")
        ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
        ("melpa"    . "http://melpa.org/packages/")
        ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))
      package-archive-priorities
      '(("gnu"      . 0)
        ("nongnu"   . 0)
        ("melpa"    . 5)
        ("jcs-elpa" . 10)))

(setq package-enable-at-startup nil  ; To avoid initializing twice
      package-check-signature nil)

(require 'package)

(when noninteractive (package--archives-initialize))
(when (featurep 'esup-child) (package-activate-all))

;;
;; (@* "Packages" )
;;

(setq use-package-always-defer t
      use-package-expand-minimally t)

(use-package pkg-dm
  :ensure t
  :hook (package-menu-mode . pkg-dm-mode)
  :init
  (setq pkg-dm-package-list
        '( 0xc
           actionscript-mode
           ada-mode
           adaptive-wrap
           adoc-mode
           alt-codes
           annotate
           ansi-colorful
           ansible
           ansible-doc
           apache-mode
           applescript-mode
           apt-sources-list
           arduino-mode
           ascii-table
           asoc
           aurora-config-mode
           auth-source-keytar
           auto-close-block
           auto-highlight-symbol
           auto-read-only
           auto-rename-tag
           auto-scroll-bar
           autotetris-mode
           balanced-windows
           banner-comment
           basic-mode
           beancount
           better-scroll
           bison-mode
           block-travel
           breadcrumb
           browse-kill-ring
           buffer-menu-filter
           buffer-move
           buttercup
           caddyfile-mode
           calfw
           caml
           cargo-mode
           cask-mode
           ccls
           centaur-tabs
           chatgpt-sideline
           cisco-router-mode
           clean-buffers
           clhs
           cmake-font-lock
           cobol-mode
           codegpt
           codemetrics
           coffee-mode
           cogru
           colorful-mode
           com-css-sort
           command-log-mode
           common-lisp-snippets
           company-ansible
           company-auctex
           company-autoconf
           company-bibtex
           company-box
           company-bootstrap
           company-c-headers
           company-cabal
           company-cmd
           company-coffee
           company-dict
           company-dockerfile
           company-eask
           company-elisp-keywords
           company-emmet
           company-emojify
           company-fuzzy
           company-glsl
           company-go
           company-kaomoji
           company-ledger
           company-lua
           company-makefile
           company-math
           company-meta-net
           company-mlton
           company-nginx
           company-nixos-options
           company-org-block
           company-paths
           company-php
           company-plsense
           company-powershell
           company-reftex
           company-restclient
           company-shell
           company-sourcekit
           company-tailwindcss
           company-terraform
           composer
           comware-router-mode
           consult-todo
           crux
           crystal-mode
           csound-mode
           csproj-mode
           css-eldoc
           cuda-mode
           cycle-at-point
           cycle-case-style
           cycle-quotes
           cycle-slash
           cython-mode
           d-mode
           dashboard-ls
           diff-hl
           diminish-buffer
           diredfl
           dist-file-mode
           docker
           docker-compose-mode
           dotenv-mode
           dumb-jump
           earthfile-mode
           easky
           ecukes
           editorconfig-generate
           el-mock
           eldoc-cmake
           eldoc-eask
           eldoc-meta-net
           eldoc-toml
           electric-cursor
           electric-indent-sexp
           elfeed
           elisp-def
           elisp-demos
           elixir-mode
           elm-mode
           emacsql-mysql
           emacsql-psql
           emoji-github
           emp
           envrc
           eping
           erlang
           eros
           eshell-syntax-highlighting
           ess
           esup
           eval-mark
           exec-path-from-shell
           execrun
           expand-region
           fasm-mode
           fb2-reader
           feature-mode
           ff-guard
           file-info
           fish-mode
           flutter
           flx-rs
           flycheck-actionlint
           flycheck-cask
           flycheck-clang-analyzer
           flycheck-clojure
           flycheck-credo
           flycheck-crystal
           flycheck-cython
           flycheck-dart
           flycheck-deno
           flycheck-eask
           flycheck-elm
           flycheck-elsa
           flycheck-golangci-lint
           flycheck-grammarly
           flycheck-haskell
           flycheck-hl-todo
           flycheck-kotlin
           flycheck-languagetool
           flycheck-ledger
           flycheck-nim
           flycheck-ocaml
           flycheck-package
           flycheck-relint
           flycheck-swift
           flycheck-ziglint
           flymake-coffee
           flymake-haml
           flymake-less
           flymake-lua
           flymake-nasm
           flymake-php
           flymake-racket
           flymake-ruby
           flymake-shell
           fof
           font-lock-ext
           forge
           fountain-mode
           free-keys
           freeradius-mode
           fsharp-mode
           fstar-mode
           fvwm-mode
           gas-mode
           gcmh
           gdscript-mode
           git-assembler-mode
           git-link
           git-modes
           github-browse-file
           github-tags
           gitignore-templates
           gitlab-ci-mode-flycheck
           gl-conf-mode
           google-this
           goto-char-preview
           goto-last-change
           goto-line-preview
           gptscript-mode
           graphql-mode
           guard-lf
           haml-mode
           hammy
           haxe-mode
           helafy
           helpful
           hexo
           hgignore-mode
           highlight-doxygen
           highlight-escape-sequences
           highlight-indent-guides
           highlight-numbers
           hl-preproc
           hlsl-mode
           htmltagwrap
           hy-mode
           ialign
           ic
           idris-mode
           iedit
           impatient-showdown
           ini-mode
           isearch-project
           jai-mode
           javadoc-lookup
           javap-mode
           jayces-mode
           jcs-echobar
           jcs-frametitle
           jcs-modeline
           jcs-poptip
           jcs-template
           jenkinsfile-mode
           json-mode
           jsonnet-mode
           k8s-mode
           kconfig-mode
           keypression
           kotlin-mode
           kubernetes
           ledger-mode
           license-templates
           line-reminder
           list-environment
           literate-calc-mode
           llvm-mode
           logms
           logview
           lsp-dart
           lsp-grammarly
           lsp-haskell
           lsp-java
           lsp-julia
           lsp-latex
           lsp-ltex
           lsp-metals
           lsp-mssql
           lsp-p4
           lsp-pascal
           lsp-pyright
           lsp-scheme
           lsp-shader
           lsp-sonarlint
           lsp-sourcekit
           lsp-tailwindcss
           magit-todos
           makefile-executor
           manage-minor-mode-table
           marginalia
           markdown-toc
           masm-mode
           merlin-company
           merlin-eldoc
           mermaid-mode
           meson-mode
           message-clean-mode
           meta-view
           minimap
           mint-mode
           moom
           most-used-words
           move-text
           multi-shell
           nasm-mode
           nerd-icons-archive
           nerd-icons-buffer-menu
           nerd-icons-completion
           nerd-icons-dired
           nerd-icons-ibuffer
           nginx-mode
           nim-mode
           ninja-mode
           nix-mode
           nocomments-mode
           noir-mode
           nov
           npm-mode
           on
           opencl-mode
           org-fancy-priorities
           org-sticky-header
           org-superstar
           organize-imports-java
           package-build
           page-break-lines
           pangu-spacing
           parse-it
           password-generator
           password-mode
           phpt-mode
           pip-requirements
           pkg-dm
           pkgbuild-mode
           po-mode
           powershell
           powerthesaurus
           prettier
           processing-mode
           project-abbrev
           prometheus-mode
           protobuf-mode
           purescript-mode
           python-mode
           qml-mode
           qss-mode
           qt-pro-mode
           quelpa-use-package
           quickrun
           racket-mode
           rainbow-csv
           region-occurrences-highlighter
           region-state
           repos-window
           responsive-window
           restart-emacs
           robots-txt-mode
           scad-mode
           scrollable-quick-peek
           scss-mode
           shader-mode
           shell-pop
           shift-number
           show-eof-mode
           shrink-whitespace
           sideline-blame
           sideline-color
           sideline-eglot
           sideline-flycheck
           sideline-flymake
           sideline-load-cost
           sideline-lsp
           site-lisp
           slim-mode
           sln-mode
           sly-asdf
           sly-macrostep
           sly-overlay
           sly-quicklisp
           sly-repl-ansi-color
           smart-comment
           smex
           sml-mode
           snow
           sort-words
           sql-indent
           ssh-config-mode
           suggest
           svelte-mode
           swift-mode
           terminal-here
           toc-org
           togetherly
           toggle-profiler
           toggle-window
           transpose-frame
           transwin
           tree-sitter-indent
           tree-sitter-langs
           treemacs-nerd-icons
           ts-docstr
           turbo-log
           typescript-mode
           typst-mode
           undo-tree-vf
           unfill
           use-ttf
           vbs-repl
           vbscript-mode
           vc-refresh
           vertico-flx
           vimrc-mode
           visual-regexp
           vs-comment-return
           visual-basic-mode
           vs-dark-theme
           vs-edit-mode
           vs-electric-spacing
           vs-light-theme
           vs-revbuf
           vsc-edit-mode
           vsc-multiple-cursors
           vue-mode
           wat-mode
           web-mode
           which-key
           whitespace-cleanup-mode
           whole-line-or-region
           winum
           yarn-mode
           yasnippet-snippets
           yuck-mode
           zoom-window)))

(require 'elenv)
(pkg-dm-install-all)

(provide 'jcs-package)
;;; jcs-package.el ends here
