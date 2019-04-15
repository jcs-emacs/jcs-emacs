;;; jcs-package.el --- Package archive related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; ==================
;; [IMPORTANT] This should be ontop of all require packages!!!

;; start package.el with emacs
(require 'package)

;; NOTE(jenchieh): Add `GNU', `MELPA', `Marmalade', `ELPA' to repository list
(progn
  ;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  ;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  )

;; To avoid initializing twice
(setq package-enable-at-startup nil)

;; initialize package.el
(package-initialize)

;; NOTE(jenchieh): This actually worsen the startup time.
;;(package-refresh-contents)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

;; Ensure all the package installed
;; SOURCE: http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(defun jcs-ensure-package-installed (packages &optional without-asking)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if without-asking
           (package-install package)
         (if (y-or-n-p (format "Package %s is missing. Install it? " package))
             (package-install package)
           package))))
   packages))


;; List of package you want to installed.
(defvar jcs-package-install-list '(ace-window
                                   actionscript-mode
                                   adaptive-wrap
                                   ag
                                   all-the-icons
                                   apache-mode
                                   auto-highlight-symbol
                                   auto-package-update
                                   auto-rename-tag
                                   beacon
                                   basic-mode
                                   better-defaults
                                   buffer-move
                                   clojure-mode
                                   cmake-font-lock
                                   cmake-ide
                                   cmake-mode
                                   cmake-project
                                   cobol-mode
                                   com-css-sort
                                   company
                                   company-quickhelp
                                   cpputils-cmake
                                   csharp-mode
                                   dash
                                   dashboard
                                   diminish
                                   dimmer
                                   elixir-mode
                                   emmet-mode
                                   erlang
                                   exec-path-from-shell
                                   floobits
                                   focus
                                   flycheck
                                   flycheck-popup-tip
                                   git-link
                                   git-messenger
                                   git-timemachine
                                   gitattributes-mode
                                   gitconfig-mode
                                   gitignore-mode
                                   glsl-mode
                                   go-mode
                                   google-maps
                                   google-this
                                   google-translate
                                   goto-line-preview
                                   haskell-mode
                                   helm
                                   helm-ag
                                   helm-gtags
                                   htmltagwrap
                                   indent-info
                                   ini-mode
                                   javadoc-lookup
                                   ;;;
                                   ;; TEMPORARY(jenchieh): Hopefully melpa will let me push
                                   ;; my package `jayces-mode' to their package system.
                                   ;; Then we can add this line under directly.
                                   ;;
                                   ;;jayces-mode
                                   ;;;
                                   ;; TEMPORARY(jenchieh): Hopefully melpa will let me push
                                   ;; my package `jcs-ex-pkg' to their package system.
                                   ;; Then we can add this line under directly.
                                   ;;
                                   ;;jcs-ex-pkg
                                   js2-mode
                                   json-mode
                                   line-reminder
                                   lua-mode
                                   magit
                                   markdown-mode
                                   markdown-preview-mode
                                   move-text
                                   multiple-cursors
                                   nasm-mode
                                   nhexl-mode
                                   organize-imports-java
                                   origami
                                   package-build
                                   package-lint
                                   pdf-tools
                                   powerline
                                   processing-mode
                                   project-abbrev
                                   projectile
                                   python-mode
                                   rainbow-mode
                                   restart-emacs
                                   right-click-context
                                   rust-mode
                                   scala-mode
                                   shader-mode
                                   sr-speedbar
                                   ssass-mode
                                   scss-mode
                                   sublimity
                                   sql-indent
                                   tabbar
                                   togetherly
                                   typescript-mode
                                   undo-tree
                                   use-package
                                   use-ttf
                                   vimrc-mode
                                   visual-regexp
                                   impatient-mode
                                   web-mode
                                   which-key
                                   wgrep-ag
                                   wgrep-helm
                                   xwidgete
                                   yaml-mode
                                   yasnippet
                                   yasnippet-snippets)
  "List of packages this config needs.")


;;;###autoload
(defun jcs-package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  ;; SOURCE(jenchieh): https://emacs.stackexchange.com/questions/16398/noninteractively-upgrade-all-packages
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
                (package-install package-desc)
                (package-delete  old-package))))
          (message "Done upgrading all packages"))
      (message "All packages are up to date"))))

;; NOTE(jenchieh): Only in Emacs 25.1+
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


(provide 'jcs-package)
;;; jcs-package.el ends here
