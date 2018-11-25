;; ========================================================================
;; $File: jcs-package.el $
;; $Date: 2018-05-15 16:37:28 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


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
(defvar jcs-package-install-list '(ac-emmet ac-html            ;; auto complete html
                                   ac-js2
                                   ac-php             ;; auto complete php
                                   ace-window
                                   actionscript-mode
                                   adaptive-wrap
                                   ag
                                   all-the-icons
                                   apache-mode
                                   auto-complete
                                   auto-complete-c-headers
                                   auto-highlight-symbol
                                   auto-package-update
                                   basic-mode
                                   better-defaults
                                   clojure-mode
                                   cmake-font-lock
                                   cmake-ide
                                   cmake-mode
                                   cmake-project
                                   cobol-mode
                                   com-css-sort
                                   company
                                   cpputils-cmake
                                   csharp-mode
                                   dash
                                   diminish
                                   ein
                                   elpy
                                   emmet-mode
                                   exec-path-from-shell
                                   find-file-in-project
                                   floobits
                                   flycheck
                                   flymake-cursor
                                   flymake-easy
                                   flymake-google-cpplint
                                   git-link
                                   git-messenger
                                   git-timemachine
                                   gitattributes-mode
                                   gitconfig-mode
                                   github-notifier
                                   gitignore-mode
                                   gitlab
                                   go-mode
                                   google-c-style
                                   google-maps
                                   google-this
                                   google-translate
                                   haskell-mode
                                   helm
                                   helm-ag
                                   helm-gtags
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
                                   jdee
                                   js2-mode
                                   js2-refactor
                                   json-mode
                                   line-reminder
                                   lua-mode
                                   magit
                                   markdown-mode
                                   markdown-preview-mode
                                   meghanada
                                   multiple-cursors
                                   nasm-mode
                                   nhexl-mode
                                   organize-imports-java
                                   package-build
                                   package-lint
                                   pdf-tools
                                   php-auto-yasnippets
                                   powerline
                                   processing-mode
                                   project-abbrev
                                   py-autopep8
                                   python-mode
                                   rainbow-mode
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
                                   yasnippet)
  "List of packages this config needs.")


;;;###autoload
(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  ;; SOURCE(jenchieh): https://emacs.stackexchange.com/questions/16398/noninteractively-upgrade-all-packages
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
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

;; NOTE(jenchieh): Only in Emacs 25.1+
;;;###autoload
(defun package-menu-filter-by-status (status)
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
