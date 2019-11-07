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

;;-----------------------------------------------------------
;;-----------------------------------------------------------

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
    browse-kill-ring
    centaur-tabs
    clojure-mode
    cmake-font-lock
    cmake-mode
    cobol-mode
    com-css-sort
    company
    company-fuzzy
    company-quickhelp
    csharp-mode
    dap-mode
    dart-mode
    dash
    dashboard
    diminish
    diminish-buffer
    dimmer
    dockerfile-mode
    dumb-jump
    elisp-def
    elixir-mode
    emmet-mode
    erlang
    ess
    esup
    exec-path-from-shell
    feebleline
    focus
    flx
    flycheck
    flycheck-popup-tip
    gdscript-mode
    gitattributes-mode
    gitconfig-mode
    gitignore-mode
    glsl-mode
    go-mode
    google-this
    google-translate
    goto-char-preview
    goto-line-preview
    haskell-mode
    haxe-mode
    helm
    helm-ag
    helm-core
    helm-describe-modes
    helm-file-preview
    helm-fuzzy
    helm-projectile
    highlight-indent-guides
    hl-todo
    htmltagwrap
    iedit
    indicators
    ini-mode
    isearch-project
    javadoc-lookup
    js2-mode
    json-mode
    kotlin-mode
    line-reminder
    lsp-mode
    lua-mode
    magit
    markdown-mode
    markdown-toc
    move-text
    multiple-cursors
    nasm-mode
    nhexl-mode
    org-bullets
    organize-imports-java
    origami
    package-build
    package-lint
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
    right-click-context
    rust-mode
    scala-mode
    shader-mode
    show-eol
    sr-speedbar
    ssass-mode
    scss-mode
    sql-indent
    swift-mode
    togetherly
    typescript-mode
    undo-tree
    use-package
    use-ttf
    vimrc-mode
    visual-regexp
    vue-mode
    impatient-mode
    web-mode
    wgrep-ag
    wgrep-helm
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
              (y-or-n-p (format "Package %s is missing. Install it? " package)))
          (jcs-package-install package)
        package)))
  ;; STUDY: Not sure if you need this?
  (when (get 'jcs-package-install 'state)
    ;; activate installed packages
    (package-initialize)))

;;;###autoload
(defun jcs-package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
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
  (let ((upgrades jcs-package-manually-install-list))
    (when (yes-or-no-p
           (message "Upgrade %d package%s (%s)? "
                    (length upgrades)
                    (if (= (length upgrades) 1) "" "s")
                    (mapconcat (lambda (pkgs) (nth 0 pkgs)) upgrades ", ")))
      (require 'quelpa)
      (quelpa-upgrade))))


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


;;========================================
;;         Manually Installation
;;----------------------------------

(defconst jcs-package-manually-install-list
  '(("define-it" "jcs-elpa/define-it" "github")
    ("jayces-mode" "jcs-elpa/jayces-mode" "github")
    ("multi-shell" "jcs-elpa/multi-shell" "github")
    ("reload-emacs" "jcs-elpa/reload-emacs" "github")
    ("shift-select" "jcs-elpa/shift-select" "github"))
  "List of package that you want to manually installed.")


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
                      (y-or-n-p (format "Package %s is missing. Install it? " pkg-name)))
              (require 'quelpa)
              (quelpa recipe))))))))


(provide 'jcs-package)
;;; jcs-package.el ends here
