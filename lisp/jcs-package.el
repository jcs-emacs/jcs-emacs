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

(defconst jcs-package-install-list
  '(0xc
    actionscript-mode
    ada-mode
    adaptive-wrap
    alt-codes
    annotate
    apache-mode
    applescript-mode
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
    basic-mode
    better-scroll
    bool-flip
    browse-kill-ring
    buffer-menu-filter
    buffer-move
    buffer-wrap
    calfw
    caml
    cargo-mode
    ccls
    centaur-tabs
    clojure-mode
    cmake-font-lock
    cobol-mode
    coffee-mode
    com-css-sort
    command-log-mode
    company-autoconf
    company-box
    company-c-headers
    company-cmd
    company-coffee
    company-dockerfile
    company-emojify
    company-fuzzy
    company-glsl
    company-kaomoji
    company-makefile
    company-meta-net
    company-nginx
    company-powershell
    company-shell
    consult
    crystal-mode
    csproj-mode
    csv-mode
    d-mode
    dashboard-ls
    define-it
    diff-hl
    diminish-buffer
    diredfl
    dist-file-mode
    docker
    docker-compose-mode
    dockerfile-mode
    docstr
    dotenv-mode
    dumb-jump
    eask-mode
    echo-bar
    editorconfig
    eldoc-meta-net
    electric-cursor
    electric-indent-sexp
    elfeed
    elisp-def
    elisp-demos
    elixir-mode
    elm-mode
    emmet-mode
    emoji-github
    eping
    erlang
    eshell-syntax-highlighting
    ess
    esup
    exec-path-from-shell
    expand-region
    feature-mode
    file-header
    fill-page
    flx-rs
    flycheck-eask
    flycheck-elsa
    flycheck-grammarly
    flycheck-languagetool
    flycheck-ocaml
    flycheck-package
    flycheck-relint
    fountain-mode
    fsharp-mode
    gcmh
    gdscript-mode
    git-assembler-mode
    git-link
    git-modes
    github-browse-file
    github-tags
    gitignore-templates
    glsl-mode
    go-mode
    google-this
    goto-char-preview
    goto-line-preview
    graphql-mode
    haml-mode
    hammy
    haskell-mode
    haxe-mode
    helpful
    hexo
    hgignore-mode
    highlight-escape-sequences
    highlight-indent-guides
    highlight-numbers
    hl-preproc
    hl-todo
    hlsl-mode
    htmltagwrap
    ialign
    iedit
    impatient-showdown
    indent-control
    ini-mode
    isearch-project
    javadoc-lookup
    javap-mode
    jayces-mode
    jenkinsfile-mode
    js2-mode
    json-mode
    keypression
    kotlin-mode
    leaf
    license-templates
    line-reminder
    log4e
    logms
    logview
    lsp-dart
    lsp-docker
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
    lua-mode
    manage-minor-mode-table
    marginalia
    markdown-toc
    masm-mode
    message-clean-mode
    meta-view
    minions
    moody
    most-used-words
    move-text
    multi-shell
    multiple-cursors
    mwim
    nasm-mode
    nginx-mode
    nhexl-mode
    nim-mode
    nix-mode
    noflet
    on
    org-fancy-priorities
    org-superstar
    organize-imports-java
    package-build
    page-break-lines
    parse-it
    password-generator
    password-mode
    phpt-mode
    pip-requirements
    powershell
    preview-it
    processing-mode
    project-abbrev
    protobuf-mode
    prt
    python-mode
    qml-mode
    quelpa
    rainbow-mode
    recentf-excl
    region-occurrences-highlighter
    region-state
    restart-emacs
    reveal-in-folder
    right-click-context
    rjsx-mode
    robots-txt-mode
    rust-mode
    scala-mode
    scrollable-quick-peek
    scss-mode
    shader-mode
    shell-pop
    shift-number
    show-eol
    shrink-whitespace
    sideline-blame
    sideline-color
    sideline-flycheck
    sideline-flymake
    sideline-lsp
    smart-comment
    smex
    sort-words
    sql-indent
    suggest
    swift-mode
    toc-org
    togetherly
    toggle-quotes
    toggle-window
    transpose-frame
    transwin
    tree-sitter-indent
    tree-sitter-langs
    ts-fold
    turbo-log
    typescript-mode
    undo-tree
    unfill
    use-ttf
    vc-msg
    vertico-flx
    vimrc-mode
    visual-regexp
    vs-dark-theme
    vs-edit-mode
    vs-light-theme
    vs-revbuf
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
    zoom-window)
  "List of packages this config needs.")

;;
;; (@* "Util" )
;;

(defun jcs-package-dependency (pkg)
  "Return list of dependency from a PKG."
  (let (result (deps (jcs-package--get-reqs pkg)) dep-name)
    (dolist (dep deps)
      (setq dep-name (car dep))
      (push dep-name result)
      (nconc result (jcs-package-dependency dep-name)))
    (cl-remove 'emacs result)))

(defun jcs-package-dependency-list (lst)
  "Return full dependency list from LST of package."
  (let (result)
    (dolist (pkg lst)
      (setq result (append result (jcs-package-dependency pkg))))
    (reverse (delete-dups result))))

(defun jcs-package-unused-packages ()
  "Return a list of unused packages."
  (let* ((installed-pkgs (jcs-package--get-selected-packages))
         (pkg-install-lst jcs-package-install-list)
         (deps (jcs-package-dependency-list pkg-install-lst))
         (full-pkgs (delete-dups (append pkg-install-lst deps)))
         unused-lst)
    (dolist (pkg installed-pkgs)
      (unless (memq pkg full-pkgs) (push pkg unused-lst)))
    (cl-remove 'emacs (reverse unused-lst))))

(defun jcs-package-desc (name &optional current)
  "Build package description by NAME."
  (cadr (assq name (if current package-alist package-archive-contents))))

(defun jcs-package--get-reqs (name)
  "Return requires from package NAME."
  (ignore-errors (package-desc-reqs (jcs-package-desc name t))))

(defun jcs-package--package-status (name)
  "Get package status by NAME."
  (let* ((desc (jcs-package-desc name t))
         (status (ignore-errors (package-desc-status desc))))
    (or status "")))

(defun jcs-package--used-elsewhere-p (name)
  "Return non-nil if NAME is used elsewhere."
  (let ((desc (jcs-package-desc name t)))
    (ignore-errors (package--used-elsewhere-p desc nil 'all))))

(defun jcs-package--package-status-p (name status)
  "Check if NAME status the same as STATUS."
  (string= (jcs-package--package-status name) status))

(defun jcs-package--package-obsolete-p (name)
  "Return non-nil if NAME is obsolete package."
  (jcs-package--package-status-p name "obsolete"))

(defun jcs-package-incompatible-p (name)
  "Return non-nil if NAME is incompatible package."
  (jcs-package--package-status-p name "incompatible"))

(defun jcs-package--package-do-rebuild (name)
  "Return non-nil if NAME suppose to be rebuild."
  (and (not (jcs-package--package-obsolete-p name))
       (not (package-built-in-p name))
       (not (jcs-package-incompatible-p name))))

;;
;; (@* "Dependency" )
;;

(defun jcs-package--filter-installed (lst)
  "Remove package from LST if not installed."
  (cl-remove-if-not (lambda (elm) (package-installed-p elm)) lst))

(defun jcs-package--get-selected-packages ()
  "Return selected packages base on the execution's condition."
  (jcs-package--filter-installed package-activated-list))

(defun jcs-package-installed-list ()
  "Return full installed package list, including builtins."
  (let (builtins)
    (setq package-activated-list (jcs-package--filter-installed package-activated-list))
    (dolist (desc package--builtins) (push (nth 0 desc) builtins))
    (cl-delete-duplicates (append builtins package-activated-list))))

(defun jcs-package-rebuild-dependency-list ()
  "Rebuild dependency graph and save to list."
  (interactive)
  (require 'jcs-util)
  (package-initialize)
  (prt-with "Building dependency graph... "
    (recentf-excl-it
      (let ((new-selected-pkg (jcs-package--get-selected-packages))
            (installed-list (jcs-package-installed-list)))
        (dolist (name installed-list)
          (if (package-installed-p name)
              (when (jcs-package--package-do-rebuild name)
                (prt-update rt (format " `%s`" name))
                (if (jcs-package--used-elsewhere-p name)
                    (setq new-selected-pkg (remove name new-selected-pkg))
                  (push name new-selected-pkg)))
            (setq new-selected-pkg (remove name new-selected-pkg))))
        (delete-dups new-selected-pkg)
        (setq new-selected-pkg (sort new-selected-pkg #'string-lessp))
        (if (equal new-selected-pkg package-selected-packages)
            (prt-done rt "No need to update dependency graph")
          (package--save-selected-packages new-selected-pkg)
          (prt-done rt "Done rebuild dependency graph"))))))

(defun jcs-package--menu-execute--advice-around (fnc &rest args)
  "Execution around function `package-menu-execute' with FNC and ARGS."
  (let (jcs-package-use-real-delete-p)
    (when (apply fnc args) (jcs-package-rebuild-dependency-list))))

(advice-add 'package-menu-execute :around #'jcs-package--menu-execute--advice-around)

;;
;; (@* "Installation" )
;;

(defconst jcs-package--elpa-temp-dir (concat user-emacs-directory "elpa/.temp/")
  "Temporary directory to mark packages so it can be deleted afterward.")

(ignore-errors (delete-directory jcs-package--elpa-temp-dir t))

(defvar jcs-package-installing-p nil
  "Is currently upgrading the package.")

(defvar jcs-package-use-real-delete-p t
  "Flag to check if we are really deleting a package.")

(defun jcs-package-delete (desc &optional dep)
  "Safe way to remove package and it's DEP using PKG-DESC."
  (let ((name (package-desc-name desc))
        (used-elsewhere (package--used-elsewhere-p desc nil 'all)))
    (dolist (tmp-desc used-elsewhere) (jcs-package-delete tmp-desc name))
    (when desc
      (let ((jcs-package-use-real-delete-p t))
        (when (msgu-silent (package-delete desc))
          (if dep (message "Delete package `%s` that is rely on package `%s`" name dep)
            (message "Package `%s` deleted." name)))))))

(defun jcs--package-delete--advice-around (fnc &rest args)
  "Execution run around function `package-delete' with FNC and ARGS."
  (let ((desc (nth 0 args)))
    (if jcs-package-use-real-delete-p
        (if-let ((result (ignore-errors (apply fnc args)))) result
          (when-let* ((pkg-dir (package-desc-dir desc))
                      (name (package-desc-name desc))
                      ((jcs-move-path pkg-dir jcs-package--elpa-temp-dir)))
            (msgu-unsilent
              (message "[INFO] Package `%s` in used, mark `%s` for later deletion"
                       name (file-name-nondirectory pkg-dir)))))
      (jcs-package-delete desc))))

(advice-add 'package-delete :around #'jcs--package-delete--advice-around)

(defun jcs--package-install--advice-around (fnc &rest args)
  "Advice around execute `package-install' command with FNC and ARGS."
  (let ((jcs-package-installing-p t)) (apply fnc args)))

(advice-add 'package-install :around #'jcs--package-install--advice-around)
(advice-add 'package-install-from-buffer :around #'jcs--package-install--advice-around)

(defun jcs-package-install (pkg)
  "Install PKG package."
  (unless (package-installed-p pkg)
    (unless package-archive-contents (package-refresh-contents))
    (package-install pkg)))

(defun jcs-ensure-package-installed (packages)
  "Assure every PACKAGES is installed."
  (mapc #'jcs-package-install packages)
  ;; Rebuild after done the installation
  (when package-archive-contents
    (jcs-package-rebuild-dependency-list)
    (package-initialize)))

(defun jcs-package-install-all ()
  "Install all needed packages from this configuration."
  (interactive)
  (jcs-ensure-package-installed jcs-package-install-list))

(defun jcs-package--show-upgrades ()
  "Show upgradable packages in one menu."
  (advice-remove 'package-menu--mark-upgrades-1 #'jcs-package--show-upgrades)
  (if (ignore-errors (package-menu-filter-upgradable))
      (progn
        (package-menu-mark-upgrades)
        (msgu-current "Press `x` to execute command; press `u` to unmark packages"))
    (message "All packages are up to date")))

(defun jcs-package-upgrade-all ()
  "Upgrade for archive packages."
  (interactive)
  (package-menu-mark-upgrades)
  (if package-menu--mark-upgrades-pending
      (advice-add 'package-menu--mark-upgrades-1 :after #'jcs-package--show-upgrades)
    (jcs-package--show-upgrades)))

(defun jcs-package-autoremove ()
  "Remove packages that are no longer needed."
  (interactive)
  (if-let ((removable (jcs-package-unused-packages)))
      (when (y-or-n-p
             (format "Packages to delete: %d (%s), proceed? "
                     (length removable)
                     (mapconcat #'symbol-name removable ", ")))
        (mapc (lambda (p)
                (package-delete (cadr (assq p package-alist)) t))
              removable)
        (jcs-package-rebuild-dependency-list))
    (message "Nothing to autoremove")))

(provide 'jcs-package)
;;; jcs-package.el ends here
