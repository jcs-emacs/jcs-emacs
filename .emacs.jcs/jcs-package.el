;;; jcs-package.el --- Package archive related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ==================
;; [IMPORTANT] This should be ontop of all require packages!!!

;; start package.el with emacs
(require 'package)

;; NOTE: Add `GNU', `MELPA', `Marmalade', `ELPA' to repository list
(setq package-archives
      '(("celpa" . "https://celpa.conao3.com/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ;;("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; To avoid initializing twice
(setq package-enable-at-startup nil)

;; Disable check signature while installing packages.
(setq package-check-signature nil)

;; initialize package.el
(when (featurep 'esup-child)
  (package-initialize))

;;
;; (@* "Packages" )
;;

;; List of package you want to installed.
(defconst jcs-package-install-list
  '(0xc
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
    yasnippet-snippets)
  "List of packages this config needs.")

(defconst jcs-package-pinned
  '((company-box . celpa)
    (origami     . celpa))
  "List of pinned packages to a specific source.")

;;
;; (@* "Util" )
;;

(defun jcs-package-dependency (pkg)
  "Return list of dependency from a PKG."
  (require 'cl-lib)
  (let (result (deps (jcs-package--get-reqs pkg)) dep-name)
    (dolist (dep deps)
      (setq dep-name (car dep))
      (push dep-name result)
      (setq result (append result (jcs-package-dependency dep-name))))
    (cl-remove 'emacs result)))

(defun jcs-package-dependency-list (lst)
  "Return full dependency list from LST of package."
  (let (result)
    (dolist (pkg lst)
      (setq result (append result (jcs-package-dependency pkg))))
    (reverse (delete-dups result))))

(defun jcs-package-unused-packages ()
  "Return a list of unused packages."
  (require 'cl-lib)
  (let* ((installed-pkgs (jcs-package--get-selected-packages))
         (pkg-install-lst (append jcs-package-install-list
                                  (jcs-package-manual-install-packages)))
         (deps (jcs-package-dependency-list pkg-install-lst))
         (full-pkgs (delete-dups (append pkg-install-lst deps)))
         unused-lst)
    (dolist (pkg installed-pkgs)
      (unless (jcs-contain-list-symbol full-pkgs pkg)
        (push pkg unused-lst)))
    (cl-remove 'emacs (reverse unused-lst))))

(defun jcs-package--add-selected-packages (pkg-name)
  "Add PKG-NAME to the selected package list."
  (unless (memq pkg-name package-selected-packages)
    (jcs-mute-apply
      (package--save-selected-packages (cons pkg-name package-selected-packages)))))

(defun jcs-package--remove-selected-packages (pkg-name)
  "Remove PKG-NAME from the selected package list."
  (when (memq pkg-name package-selected-packages)
    (jcs-mute-apply
      (package--save-selected-packages (remove pkg-name package-selected-packages)))))

(defun jcs-package---build-desc-by-archive (pkg archive)
  "Return package-desc by PKG and ARCHIVE."
  (require 'cl-lib)
  (cl-some
   (lambda (desc)
     (when (eq archive (ignore-errors (intern (package-desc-archive desc))))
       desc))
   (assq pkg package-archive-contents)))

(defun jcs-package--build-desc (pkg-name &optional archive)
  "Build package description by PKG-NAME."
  (if archive (jcs-package---build-desc-by-archive pkg-name archive)
    (or (cadr (assq pkg-name package-alist))
        (cadr (assq pkg-name package-archive-contents)))))

(defun jcs-package--get-reqs (name)
  "Return requires from package NAME."
  (ignore-errors (package-desc-reqs (jcs-package--build-desc name))))

(defun jcs-package--get-req (key name)
  "Return KEY requires from package NAME."
  (ignore-errors (assoc key (jcs-package--get-reqs name))))

(defun jcs-package--package-name (pkg-desc)
  "Return package name from PKG-DESC."
  (when (package-desc-p pkg-desc) (aref pkg-desc 1)))

(defun jcs-package--package-status (pkg-name)
  "Get package status by PKG-NAME."
  (let* ((desc (jcs-package--build-desc pkg-name))
         (status (ignore-errors (package-desc-status desc))))
    (or status "")))

(defun jcs-package--used-elsewhere-p (pkg-name)
  "Return non-nil if PKG-NAME is used elsewhere."
  (let ((desc (jcs-package--build-desc pkg-name)))
    (ignore-errors (package--used-elsewhere-p desc nil 'all))))

(defun jcs-package--package-status-p (pkg-name status)
  "Check if PKG-NAME status the same as STATUS."
  (string= (jcs-package--package-status pkg-name) status))

(defun jcs-package--package-obsolete-p (pkg-name)
  "Return non-nil if PKG-NAME is obsolete package."
  (jcs-package--package-status-p pkg-name "obsolete"))

(defun jcs-package-incompatible-p (pkg-name)
  "Return non-nil if PKG-NAME is incompatible package."
  (jcs-package--package-status-p pkg-name "incompatible"))

(defun jcs-package--package-do-rebuild (pkg-name)
  "Return non-nil if PKG-NAME suppose to be rebuild."
  (and (not (jcs-package--package-obsolete-p pkg-name))
       (not (package-built-in-p pkg-name))
       (not (jcs-package-incompatible-p pkg-name))))

;;
;; (@* "Dependency" )
;;

(defvar jcs-package-rebuild-dependency-p t
  "Flag to see if able to rebuild dependency graph at the moment.")

(defvar jcs-package--need-rebuild-p nil
  "Flag to see if we need to rebuild for the next command.")

(defun jcs-package--filter-installed (lst)
  "Remove package from LST if not installed."
  (require 'cl-lib)
  (cl-remove-if-not (lambda (elm) (jcs-package-installed-p elm)) lst))

(defun jcs-package--get-selected-packages ()
  "Return selected packages base on the execution's condition."
  (jcs-package--filter-installed package-activated-list))

(defun jcs-package-installed-list ()
  "Return full installed package list, including builtins."
  (let (builtins)
    (setq package-activated-list (jcs-package--filter-installed package-activated-list))
    (dolist (pkg-desc package--builtins) (push (nth 0 pkg-desc) builtins))
    (cl-delete-duplicates (append builtins package-activated-list))))

;;;###autoload
(defun jcs-package-rebuild-dependency-list ()
  "Rebuild dependency graph and save to list."
  (interactive)
  (package-initialize)
  (if (not jcs-package-rebuild-dependency-p)
      (setq jcs-package--need-rebuild-p t)
    (jcs-process-reporter-start "Building dependency graph...")
    (let ((new-selected-pkg (jcs-package--get-selected-packages))
          (installed-list (jcs-package-installed-list)))
      (dolist (pkg-name installed-list)
        (if (package-installed-p pkg-name)
            (when (jcs-package--package-do-rebuild pkg-name)
              (jcs-process-reporter-update (format "Build for package `%s`" pkg-name))
              (if (jcs-package--used-elsewhere-p pkg-name)
                  (setq new-selected-pkg (remove pkg-name new-selected-pkg))
                (push pkg-name new-selected-pkg)))
          (setq new-selected-pkg (remove pkg-name new-selected-pkg))))
      (delete-dups new-selected-pkg)
      (setq new-selected-pkg (sort new-selected-pkg #'string-lessp))
      (if (equal new-selected-pkg package-selected-packages)
          (jcs-process-reporter-done "No need to update dependency graph")
        (package--save-selected-packages new-selected-pkg)
        (jcs-process-reporter-done "Done rebuild dependency graph")))))

(defun jcs-package--menu-execute--advice-around (fnc &rest args)
  "Execution around function `package-menu-execute' with FNC and ARGS."
  (let ((jcs-package-use-real-delete-p nil))
    (when (apply fnc args)
      (jcs-package-rebuild-dependency-list)
      (jcs-dashboard-safe-refresh-buffer t))))

(advice-add 'package-menu-execute :around #'jcs-package--menu-execute--advice-around)

;;
;; (@* "Core Installation" )
;;

(defvar jcs-package-use-real-delete-p t
  "Flag to check if we are really deleting a package.")

(defun jcs-package-delete (pkg-name &optional dep)
  "Safe way to remove PKG-NAME and it's DEP."
  (let ((used-elsewhere (jcs-package--used-elsewhere-p pkg-name))
        (pkg-desc-current (jcs-package--build-desc pkg-name)))
    (dolist (pkg-desc used-elsewhere)
      (jcs-package-delete (jcs-package--package-name pkg-desc) pkg-name))
    (when pkg-desc-current
      (let ((jcs-package-use-real-delete-p t))
        (jcs-mute-apply (package-delete pkg-desc-current)))
      (if dep (message "Delete package `%s` that is rely on package `%s`" pkg-name dep)
        (message "Package `%s` deleted." pkg-name)))))

(defun jcs--package-delete--advice-around (fnc &rest args)
  "Execution run around function `package-delete' with FNC and ARGS."
  (if jcs-package-use-real-delete-p (apply fnc args)
    (jcs-package-delete (jcs-package--package-name (nth 0 args)))))

(advice-add 'package-delete :around #'jcs--package-delete--advice-around)

(defvar jcs-package-installing-p nil
  "Is currently upgrading the package.")

(defun jcs--package-install--advice-around (fnc &rest args)
  "Advice around execute `package-install' command with FNC and ARGS."
  (let ((jcs-package-installing-p t)) (apply fnc args)))

(advice-add 'package-install :around #'jcs--package-install--advice-around)
(advice-add 'package-install-from-buffer :around #'jcs--package-install--advice-around)

(defvar jcs-package--install-on-start-up nil
  "Return non-nil if installation is occurred on start-up.")

(defun jcs-package-installed-p (pkg)
  "Return non-nil if PKG is already installed."
  (or (package-installed-p pkg) (package-built-in-p pkg)))

(defun jcs-package--pinned-p (pkg)
  "Return non-nil if PKG is pinned."
  (require 'cl-lib)
  (cl-some (lambda (pin-pkg) (when (eq (car pin-pkg) pkg) pin-pkg)) jcs-package-pinned))

(defun jcs--package-download-transaction--advice-before (pkgs)
  "Execution runs before function `package-download-transaction', PKGS."
  (let ((index 0) name pin)
    (dolist (pkg pkgs)
      (setq name (jcs-package--package-name pkg)
            pin (jcs-package--pinned-p name))
      (when pin
        (setf (nth index pkgs) (jcs-package--build-desc name (cdr pin))))
      (setq index (1+ index))))
  pkgs)

(advice-add 'package-download-transaction :before #'jcs--package-download-transaction--advice-before)

(defun jcs-package-install (pkg)
  "Install PKG package."
  (unless (jcs-package-installed-p pkg)
    (setq jcs-package--install-on-start-up t)
    ;; Don't run `package-refresh-contents' if you don't need to install
    ;; packages on startup.
    (package-refresh-contents)
    ;; Else we just install the package regularly.
    (package-install pkg)))

(defun jcs-ensure-package-installed (packages)
  "Assure every PACKAGES is installed."
  (dolist (pkg packages) (jcs-package-install pkg))
  ;; STUDY: Not sure if you need this?
  (when jcs-package--install-on-start-up
    (jcs-package-rebuild-dependency-list)
    (package-initialize)))

(defun jcs-package-version (name where)
  "Get version of the package by NAME.

Argument WHERE is the alist of package information."
  (let* ((pin (jcs-package--pinned-p name))
         (local-p (equal where package-alist))
         (pkg (if (and pin (not local-p))
                  (jcs-package--build-desc name (cdr pin))
                (cadr (assq name where)))))
    (when pkg (package-desc-version pkg))))

(defun jcs-package-get-package-by-name (pkg-name)
  "Return the package by PKG-NAME."
  (let (target-pkg)
    (dolist (pkg (mapcar #'car package-alist))
      (when (string= pkg-name pkg) (setq target-pkg pkg)))
    (if (not target-pkg) nil
      (cadr (assq (package-desc-name
                   (cadr (assq target-pkg package-alist)))
                  package-alist)))))

(defun jcs-package--upgrade-all-elpa ()
  "Upgrade for archive packages."
  (let (upgrades)
    (dolist (pkg (mapcar #'car package-alist))
      (let ((in-archive (jcs-package-version pkg package-archive-contents))
            (pin (jcs-package--pinned-p pkg)))
        (when (and in-archive
                   (version-list-< (jcs-package-version pkg package-alist)
                                   in-archive))
          (push (if pin (jcs-package--build-desc pkg (cdr pin))
                  (cadr (assq pkg package-archive-contents)))
                upgrades))))
    (if upgrades
        (when (yes-or-no-p
               (format "[ELPA] Upgrade %d package%s (%s)? "
                       (length upgrades)
                       (if (= (length upgrades) 1) "" "s")
                       (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (jcs-package-install package-desc)
                (package-delete old-package))))
          (jcs-package-rebuild-dependency-list)
          (message "[ELPA] Done upgrading all packages"))
      (message "[ELPA] All packages are up to date"))))

(defun jcs-package--upgrade-all-quelpa ()
  "Upgrade for manually installed packages."
  (let ((upgrades (jcs--upgrade-list-manually)) desc)
    (if upgrades
        (when (yes-or-no-p
               (format "[QUELPA] Upgrade %d package%s (%s)? "
                       (length upgrades)
                       (if (= (length upgrades) 1) "" "s")
                       (mapconcat (lambda (rcp)
                                    (symbol-name (jcs--recipe-get-info rcp :name)))
                                  upgrades ", ")))
          ;; Delete all upgrading packages before installation.
          (dolist (rcp upgrades)
            (setq desc (jcs-package-get-package-by-name (jcs--recipe-get-info rcp :name)))
            (when desc (package-delete desc)))
          (jcs-ensure-manual-package-installed upgrades)
          (message "[QUELPA] Done upgrading all packages"))
      (message "[QUELPA] All packages are up to date"))))

;;;###autoload
(defun jcs-package-install-all ()
  "Install all needed packages from this configuration."
  (interactive)
  (let (jcs-package-rebuild-dependency-p jcs-package--need-rebuild-p)
    (jcs-ensure-package-installed jcs-package-install-list)
    (jcs-ensure-manual-package-installed jcs-package-manual-install-list)
    (when jcs-package--need-rebuild-p
      (setq jcs-package-rebuild-dependency-p t)
      (jcs-package-rebuild-dependency-list))))

;;;###autoload
(defun jcs-package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (jcs-package-rebuild-dependency-p jcs-package--need-rebuild-p)
    (jcs-package--upgrade-all-elpa)
    (jcs-package--upgrade-all-quelpa)
    (if (not jcs-package--need-rebuild-p)
        (jcs-sit-for)
      (setq jcs-package-rebuild-dependency-p t)
      (jcs-package-rebuild-dependency-list))))

;;;###autoload
(defun jcs-package-autoremove ()
  "Remove packages that are no longer needed."
  (interactive)
  (let ((removable (jcs-package-unused-packages)))
    (if removable
        (when (y-or-n-p
               (format "Packages to delete: %d (%s), proceed? "
                       (length removable)
                       (mapconcat #'symbol-name removable ", ")))
          (mapc (lambda (p)
                  (package-delete (cadr (assq p package-alist)) t))
                removable)
          (jcs-package-rebuild-dependency-list))
      (message "Nothing to autoremove"))))

;;;###autoload
(defun jcs-package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by STATUS."
  (interactive
   (list (completing-read
          "Status: " '(".."
                       "available"
                       "built-in"
                       "dependency"
                       "incompat"
                       "installed"
                       "new"
                       "obsolete"))))
  (pcase status
    (".." (package-list-packages))
    (_ (package-menu-filter (concat "status:" status)))))

;;
;; (@* "Manual Installation" )
;;

(defconst jcs-quelpa-recipes-dir (expand-file-name "~/.emacs.jcs/recipes/")
  "Manually installed recipes path.")

(defun jcs--quelpa-recipes ()
  "Return all `quelpa' recipes."
  (require 'jcs-file) (require 'jcs-util) (require 'thingatpt)
  (let ((rcps-ff (jcs-dir-to-filename jcs-quelpa-recipes-dir nil t)) rcps rcp)
    (dolist (rcp-file rcps-ff)
      (setq rcp
            (eval (thing-at-point--read-from-whole-string
                   (concat "'" (jcs-get-string-from-file rcp-file)))))
      (push rcp rcps))
    (reverse rcps)))

(defvar jcs-package-manual-install-list (jcs--quelpa-recipes)
  "List of package that you want to manually installed.")

(defun jcs-package-manual-install-packages ()
  "Return a list of manuall install packages."
  (let (mi-lst)
    (dolist (rcp jcs-package-manual-install-list)
      (push (nth 0 rcp) mi-lst))
    (reverse mi-lst)))

(defun jcs--form-version-recipe (rcp)
  "Create the RCP for `quelpa' version check."
  (let ((name (symbol-name (pop rcp)))) (push (make-symbol name) rcp) rcp))

(defun jcs--recipe-get-info (rcp prop)
  "Get the PROP information from RCP."
  (let ((plst rcp)) (push :name plst) (plist-get plst prop)))

(defun jcs--package-version-by-recipe (rcp)
  "Return the package version by recipe (RCP)."
  (let* ((pkg-repo (jcs--recipe-get-info rcp :repo))
         (pkg-fetcher (jcs--recipe-get-info rcp :fetcher))
         (rcp (jcs--form-version-recipe rcp))
         (name (car rcp))
         (build-dir (expand-file-name (symbol-name name) quelpa-build-dir))
         quelpa-build-verbose)
    (jcs-no-log-apply
      (message "Contacting host: '%s' from '%s'" pkg-repo pkg-fetcher))
    (jcs-mute-apply (quelpa-checkout rcp build-dir))))

(defun jcs--ver-string-to-ver-list (ver)
  "Convert VER string to version recognized list."
  (let ((str-lst (split-string (format "%s" ver) "[.]")) ver-lst)
    (dolist (ver-str str-lst) (push (string-to-number ver-str) ver-lst))
    (reverse ver-lst)))

(defun jcs--upgrade-list-manually ()
  "List of need to upgrade package from manually installed packages."
  (require 'quelpa)
  (let (upgrade-list new-version current-version pkg-name)
    (dolist (rcp jcs-package-manual-install-list)
      (setq pkg-name (jcs--recipe-get-info rcp :name)
            new-version (jcs--package-version-by-recipe rcp)
            current-version (jcs-package-version pkg-name package-alist)
            new-version (jcs--ver-string-to-ver-list new-version))
      (when (version-list-< current-version new-version) (push rcp upgrade-list)))
    (reverse upgrade-list)))

(defun jcs-ensure-manual-package-installed (packages)
  "Ensure all manually installed PACKAGES are installed."
  (let ((jcs-package-installing-p t) pkg-name pkg-repo pkg-fetcher
        quelpa-build-verbose pkg-installed-p)
    (dolist (rcp packages)
      (setq pkg-name (jcs--recipe-get-info rcp :name)
            pkg-repo (jcs--recipe-get-info rcp :repo)
            pkg-fetcher (jcs--recipe-get-info rcp :fetcher))
      (unless (jcs-package-installed-p pkg-name)
        (require 'quelpa) (require 'jcs-util)
        (jcs-no-log-apply
          (message "Installing '%s' from '%s'" pkg-repo pkg-fetcher))
        (quelpa rcp)
        (setq pkg-installed-p t)))
    (when pkg-installed-p (jcs-package-rebuild-dependency-list))))

(provide 'jcs-package)
;;; jcs-package.el ends here
