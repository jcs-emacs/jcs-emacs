;;; jcs-package.el --- Package archive related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ==================
;; [IMPORTANT] This should be ontop of all require packages!!!

;; start package.el with emacs
(require 'package)

;; NOTE: Add `GNU', `MELPA', `Marmalade', `ELPA' to repository list
(progn
  ;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  )

;; To avoid initializing twice
(setq package-enable-at-startup nil)

;; Disable check signature while installing packages.
(setq package-check-signature nil)

;; initialize package.el
(package-initialize)

;;----------------------------------------------------------------------------

;; List of package you want to installed.
(defconst jcs-package-install-list
  '(ace-window
    actionscript-mode
    adaptive-wrap
    ag
    alt-codes
    apache-mode
    auto-highlight-symbol
    auto-rename-tag
    basic-mode
    buffer-move
    buffer-wrap
    browse-kill-ring
    centaur-tabs
    clojure-mode
    cmake-font-lock
    cmake-mode
    cobol-mode
    com-css-sort
    command-log-mode
    company
    company-emoji
    company-fuzzy
    company-lsp
    company-quickhelp
    company-quickhelp-terminal
    counsel
    counsel-projectile
    csharp-mode
    csproj-mode
    dap-mode
    dart-mode
    dash
    dashboard
    dashboard-ls
    define-it
    diminish
    diminish-buffer
    dockerfile-mode
    dumb-jump
    elisp-def
    elixir-mode
    emmet-mode
    emoji-github
    emojify
    erlang
    ess
    esup
    exec-path-from-shell
    feebleline
    ffmpeg-player
    flx
    flycheck-grammarly
    flycheck-popup-tip
    flycheck-pos-tip
    gdscript-mode
    gitattributes-mode
    gitconfig-mode
    github-browse-file
    gitignore-mode
    glsl-mode
    go-mode
    google-this
    google-translate
    goto-char-preview
    goto-line-preview
    haskell-mode
    haxe-mode
    highlight-indent-guides
    hl-todo
    htmltagwrap
    ialign
    iedit
    indicators
    ini-mode
    isearch-project
    ivy
    javadoc-lookup
    js2-mode
    json-mode
    kotlin-mode
    line-reminder
    lsp-java
    lsp-mode
    lsp-origami
    lsp-ui
    lua-mode
    magit
    manage-minor-mode
    manage-minor-mode-table
    markdown-mode
    markdown-toc
    masm-mode
    move-text
    multiple-cursors
    nasm-mode
    neotree
    nhexl-mode
    org-bullets
    organize-imports-java
    origami
    package-build
    package-lint
    parse-it
    powerline
    preproc-font-lock
    processing-mode
    project-abbrev
    projectile
    python-mode
    quelpa
    rainbow-mode
    region-occurrences-highlighter
    request
    restart-emacs
    reveal-in-folder
    right-click-context
    rjsx-mode
    rust-mode
    scala-mode
    shader-mode
    show-eol
    ssass-mode
    scss-mode
    smex
    sql-indent
    swift-mode
    swiper
    togetherly
    typescript-mode
    undo-tree
    use-package
    use-ttf
    vimrc-mode
    visual-regexp
    vs-dark-theme
    vs-light-theme
    vue-mode
    impatient-mode
    web-mode
    wgrep-ag
    which-key
    wiki-summary
    yaml-mode
    yascroll
    yasnippet
    yasnippet-snippets)
  "List of packages this config needs.")

(defvar jcs-package-installing nil
  "Is currently upgrading the package.")


(defun jcs-advice-package-install-around (ori-func &rest args)
  "Advice around execute `package-install' command."
  (setq jcs-package-installing t)
  (apply ori-func args)
  (setq jcs-package-installing nil))
(advice-add 'package-install :around #'jcs-advice-package-install-around)

(defun jcs-package-install (pkg)
  "Install PKG package."
  (unless (get 'jcs-package-install 'state)
    (put 'jcs-package-install 'state t))
  ;; Don't run `package-refresh-contents' if you don't need to
  ;; install packages on startup.
  (package-refresh-contents)
  ;; Else we just install the package regularly.
  (package-install pkg))

(defun jcs-ensure-package-installed (packages &optional without-asking)
  "Assure every PACKAGES is installed, ask WITHOUT-ASKING."
  (dolist (package packages)
    (unless (package-installed-p package)
      (if (or without-asking
              (y-or-n-p (format "[ELPA] Package %s is missing. Install it? " package)))
          (jcs-package-install package)
        package)))
  ;; STUDY: Not sure if you need this?
  (when (get 'jcs-package-install 'state)
    ;; activate installed packages
    (package-initialize)))

(defun jcs-get-package-version (name where)
  "Get version of the package."
  (let ((pkg (cadr (assq name where))))
    (when pkg
      (package-desc-version pkg))))

(defun jcs-package-get-package-by-name (pkg-name)
  "Return the package by PKG-NAME."
  (let (target-pkg)
    (dolist (pkg (mapcar #'car package-alist))
      (let ((in-archive (jcs-get-package-version pkg package-alist)))
        (when (string= pkg-name pkg)
          (setq target-pkg pkg))))
    (if (not target-pkg)
        nil
      (cadr (assq (package-desc-name
                   (cadr (assq target-pkg package-alist)))
                  package-alist)))))

;;;###autoload
(defun jcs-package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (dolist (package (mapcar #'car package-alist))
      (let ((in-archive (jcs-get-package-version package package-archive-contents)))
        (when (and in-archive
                   (version-list-< (jcs-get-package-version package package-alist)
                                   in-archive))
          (push (cadr (assq package package-archive-contents))
                upgrades))))
    (if upgrades
        (when (yes-or-no-p
               (message "[ELPA] Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (jcs-package-install package-desc)
                (package-delete old-package))))
          (message "[ELPA] Done upgrading all packages"))
      (message "[ELPA] All packages are up to date")))
  ;; NOTE: Upgrade for manually installed packages.
  (let ((upgrades (jcs--upgrade-list-manually)))
    (if upgrades
        (when (yes-or-no-p
               (message "[QUELPA] Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat (lambda (pkgs) (nth 0 pkgs)) upgrades ", ")))
          ;; Delete all upgrading packages before installation.
          (dolist (pkg upgrades)
            (package-delete (jcs-package-get-package-by-name (nth 0 pkg))))
          (jcs-ensure-manual-package-installed upgrades t)
          (message "[QUELPA] Done upgrading all packages"))
      (message "[QUELPA] All packages are up to date"))))


;;;###autoload
(defun jcs-package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by status."
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
  (if (string= status "..")
      (package-list-packages)
    (package-menu-filter (concat "status:" status))))


;;----------------------------------------------------------------------------
;; Manually Installation


(defconst jcs-package-manually-install-list
  '(("file-header" "jcs-elpa/file-header" "github")
    ("jayces-mode" "jcs-elpa/jayces-mode" "github")
    ("multi-shell" "jcs-elpa/multi-shell" "github")
    ("reload-emacs" "jcs-elpa/reload-emacs" "github")
    ("transwin" "jcs-elpa/transwin" "github"))
  "List of package that you want to manually installed.")


(defun jcs--upgrade-list-manually ()
  "List of need to upgrade package from manually installed packages."
  (let ((upgrade-list '()) (pkg-name nil))
    (dolist (pkg jcs-package-manually-install-list)
      (setq pkg-name (nth 0 pkg))
      ;; TODO: Check version number to filter the needed upgrade packages.
      (when (package-installed-p (intern pkg-name))
        (push pkg upgrade-list)))
    (reverse upgrade-list)))

(defun jcs--form-recipe (name repo fetcher)
  "Create the recipe, with NAME, REPO, FETCHER."
  (let ((recipe '()))
    (push (plist-put nil ':fetcher fetcher) recipe)
    (push (plist-put nil ':repo repo) recipe)
    (push (make-symbol name) recipe)
    (require 'dash)
    (-flatten recipe)))

(defun jcs-ensure-manual-package-installed (packages &optional without-asking)
  "Ensure all manually installed PACKAGES are installed, ask WITHOUT-ASKING."
  (unless (jcs-reload-emacs-reloading-p)
    (let ((jcs-package-installing t))
      (dolist (pkg-info packages)
        (let* ((pkg-name (nth 0 pkg-info))
               (pkg-repo (nth 1 pkg-info))
               (pkg-fetcher (nth 2 pkg-info))
               (recipe (jcs--form-recipe pkg-name pkg-repo pkg-fetcher)))
          (unless (package-installed-p (intern pkg-name))
            (when (or without-asking
                      (y-or-n-p (format "[QUELPA] Package %s is missing. Install it? " pkg-name)))
              (require 'quelpa)
              (quelpa recipe))))))))

(provide 'jcs-package)
;;; jcs-package.el ends here
