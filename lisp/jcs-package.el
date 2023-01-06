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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t
      use-package-expand-minimally t)

(use-package pkg-dm
  :ensure t
  :hook (package-menu-mode . pkg-dm-mode)
  :init
  (setq pkg-dm-package-list
        '(0xc
          actionscript-mode
          ada-mode
          adaptive-wrap
          adoc-mode
          alt-codes
          annotate
          ansible
          ansible-doc
          apache-mode
          applescript-mode
          apt-sources-list
          arduino-mode
          ascii-table
          asoc
          auth-source-keytar
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
          block-travel
          browse-kill-ring
          buffer-menu-filter
          buffer-move
          calfw
          caml
          cargo-mode
          cask-mode
          ccls
          centaur-tabs
          clojure-mode
          cmake-font-lock
          cobol-mode
          coffee-mode
          com-css-sort
          command-log-mode
          company-ansible
          company-autoconf
          company-box
          company-c-headers
          company-cmd
          company-coffee
          company-dockerfile
          company-eask
          company-elisp-keywords
          company-emojify
          company-fuzzy
          company-glsl
          company-go
          company-kaomoji
          company-lua
          company-makefile
          company-meta-net
          company-nginx
          company-nixos-options
          company-paths
          company-powershell
          company-shell
          company-sourcekit
          company-terraform
          consult
          crystal-mode
          csproj-mode
          css-eldoc
          csv-mode
          cycle-at-point
          cycle-case-style
          cycle-quotes
          cycle-slash
          d-mode
          dashboard-ls
          define-it
          diff-hl
          diminish-buffer
          diredfl
          dist-file-mode
          docker
          docker-compose-mode
          dotenv-mode
          dumb-jump
          easky
          editorconfig-generate
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
          emmet-mode
          emoji-github
          emp
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
          feature-mode
          ff-guard
          file-header
          fish-mode
          flx-rs
          flycheck-cask
          flycheck-crystal
          flycheck-deno
          flycheck-eask
          flycheck-elm
          flycheck-elsa
          flycheck-golangci-lint
          flycheck-grammarly
          flycheck-haskell
          flycheck-kotlin
          flycheck-languagetool
          flycheck-nim
          flycheck-ocaml
          flycheck-package
          flycheck-relint
          flycheck-swift
          fof
          font-lock-ext
          forge
          fountain-mode
          free-keys
          fsharp-mode
          gcmh
          gdscript-mode
          git-assembler-mode
          git-link
          git-modes
          github-browse-file
          github-tags
          gitignore-templates
          gitlab-ci-mode-flycheck
          google-this
          goto-char-preview
          goto-line-preview
          graphql-mode
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
          ialign
          idris-mode
          iedit
          impatient-showdown
          ini-mode
          isearch-project
          javadoc-lookup
          javap-mode
          jayces-mode
          jcs-echobar
          jcs-frametitle
          jcs-modeline
          jenkinsfile-mode
          json-mode
          k8s-mode
          keypression
          kotlin-mode
          kubernetes
          license-templates
          line-reminder
          list-environment
          literate-calc-mode
          log4e
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
          lsp-pascal
          lsp-pyright
          lsp-sonarlint
          lsp-sourcekit
          lsp-tailwindcss
          lsp-ui
          magit-todos
          makefile-executor
          manage-minor-mode-table
          marginalia
          markdown-toc
          masm-mode
          message-clean-mode
          meta-view
          minimap
          mint-mode
          most-used-words
          move-text
          multi-shell
          nasm-mode
          nginx-mode
          nim-mode
          nix-mode
          nocomments-mode
          noflet
          nov
          npm-mode
          on
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
          powershell
          prettier
          preview-it
          processing-mode
          project-abbrev
          protobuf-mode
          purescript-mode
          python-mode
          qml-mode
          quelpa
          quickrun
          racket-mode
          rainbow-mode
          region-occurrences-highlighter
          region-state
          repos-window
          restart-emacs
          reveal-in-folder
          right-click-context
          robots-txt-mode
          rust-mode
          scad-mode
          scrollable-quick-peek
          scss-mode
          shader-mode
          shell-pop
          shift-number
          shrink-whitespace
          sideline-blame
          sideline-color
          sideline-flycheck
          sideline-flymake
          sideline-lsp
          sln-mode
          smart-comment
          smex
          snow
          sort-words
          sql-indent
          ssh-config-mode
          suggest
          swift-mode
          terminal-here
          toc-org
          togetherly
          toggle-profiler
          toggle-window
          topsy
          transpose-frame
          transwin
          tree-sitter-indent
          tree-sitter-langs
          ts-docstr
          turbo-log
          typescript-mode
          undo-tree-vf
          unfill
          use-package
          use-ttf
          vc-refresh
          vertico-flx
          vimrc-mode
          visual-regexp
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
          zig-mode
          zoom-window)))

(require 'elenv)
(pkg-dm-install-all)

(provide 'jcs-package)
;;; jcs-package.el ends here
