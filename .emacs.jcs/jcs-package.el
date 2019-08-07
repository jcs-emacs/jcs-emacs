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

;; initialize package.el
(when (version< emacs-version "27.0")
  (package-initialize))

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
    beacon
    basic-mode
    buffer-move
    centaur-tabs
    clojure-mode
    cmake-font-lock
    cmake-mode
    cobol-mode
    com-css-sort
    company
    company-quickhelp
    csharp-mode
    dart-mode
    dash
    dashboard
    diminish
    dimmer
    dumb-jump
    elixir-mode
    emmet-mode
    erlang
    esup
    exec-path-from-shell
    feebleline
    focus
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
    helm-file-preview
    helm-projectile
    highlight-indent-guides
    hl-todo
    htmltagwrap
    indicators
    ini-mode
    isearch-project
    javadoc-lookup
    js2-mode
    json-mode
    line-reminder
    lua-mode
    magit
    markdown-mode
    move-text
    multiple-cursors
    nasm-mode
    nhexl-mode
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
    rainbow-mode
    region-occurrences-highlighter
    restart-emacs
    right-click-context
    rust-mode
    scala-mode
    shader-mode
    show-eol
    sr-speedbar
    ssass-mode
    scss-mode
    sublimity
    sql-indent
    swift-mode
    togetherly
    typescript-mode
    undo-tree
    use-package
    use-ttf
    vimrc-mode
    impatient-mode
    web-mode
    which-key
    wgrep-ag
    wgrep-helm
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
  "Assure every package is installed, ask for installation if it’s not."
  (dolist (package packages)
    (unless (package-installed-p package)
      (if without-asking
          (jcs-package-install package)
        (if (y-or-n-p (format "Package %s is missing. Install it? " package))
            (jcs-package-install package)
          package))))
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
                (package-delete  old-package))))
          (message "Done upgrading all packages"))
      (message "All packages are up to date"))))


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
  '("~/.emacs.d/elisp/jayces-mode-20190620.001/"
    "~/.emacs.d/elisp/jcs-ex-pkg-20190326.001/"
    "~/.emacs.d/elisp/reload-emacs-20190326.001/"
    "~/.emacs.d/elisp/shift-select-20190423.001/")
  "List of package that you want to manually installed.")

(unless (jcs-reload-emacs-reloading-p)
  (dolist (pkg-path jcs-package-manually-install-list)
    (add-to-list 'load-path pkg-path)
    (let ((al-files (directory-files pkg-path nil "-autoloads.el")))
      (dolist (al-file al-files)
        (require (intern (file-name-sans-extension al-file)))))))


(provide 'jcs-package)
;;; jcs-package.el ends here
