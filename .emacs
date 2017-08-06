;; This is the start of .emacs file
;;------------------------------------------------------------------------------------------------------

;; This is my super-poopy .emacs file.
;; I barely know how to program LISP, and I know
;; even less about ELISP.  So take everything in
;; this file with a grain of salt!
;;
;; - Casey
;; - JenChieh (Modefied)

;; ==================
;; [IMPORTANT] This should be ontop of all require packages!!!

;; start package.el with emacs
(require 'package)

;; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; To avoid initializing twice
(setq package-enable-at-startup nil)

;; initialize package.el
(package-initialize)

;;------------------------------------------------------------------------------------------------------
;;;
;; Auto install list of packages i want at the startup of emacs.
;;;

;; Ensure all the package installed
;; Source -> http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; How to use?
(ensure-package-installed 'actionscript-mode  ;; for text related mode
                          'ac-php             ;; auto complete php
                          'ac-html            ;; auto complete html
                          'ac-js2
                          'ac-emmet
                          'ace-window
                          'ag
                          'auto-complete
                          'auto-complete-c-headers
                          'auto-install
                          'auto-package-update
                          'batch-mode
                          'better-defaults
                          'blank-mode
                          'cmake-font-lock
                          'cmake-ide
                          'cmake-mode
                          'cmake-project
                          'cpputils-cmake
                          'csharp-mode
                          'dash
                          'diminish
                          'ein
                          'elpy
                          'emmet-mode
                          'exec-path-from-shell
                          'flymake-cursor
                          'flymake-easy
                          'flymake-google-cpplint
                          'git-link
                          'git-messenger
                          'git-timemachine
                          'gitattributes-mode
                          'gitconfig-mode
                          'github-notifier
                          'gitignore-mode
                          'gitlab
                          'google-c-style
                          'helm
                          'helm-ag
                          'helm-gtags
                          'jdee
                          'js2-mode
                          'js2-refactor
                          'json-mode
                          'java-imports
                          'lua-mode
                          'multiple-cursors
                          'nasm-mode
                          'neotree
                          'php-auto-yasnippets
                          'powerline
                          'py-autopep8
                          'python-mode
                          'rainbow-mode
                          'sublimity
                          'undo-tree
                          'impatient-mode
                          'web-mode
                          'wgrep-ag
                          'wgrep-helm
                          'yasnippet)


;; activate installed packages
(package-initialize)

;;========================================
;;      JENCHIEH FILE LOADING
;;----------------------------------

;;; Environment.
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-env.el")
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-plugin.el")

;;; Customization
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-theme.el")

;;; Initialize
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-before-init.el")

;;; Utilities
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-file-info-format.el")
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-function.el")
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-helm.el")

;;; jcs-all-mode
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-elisp-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-cmake-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-nasm-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-batch-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-sh-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-cc-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-jayces-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-java-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-actionscript-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-python-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-web-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-js-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-json-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-lua-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-cs-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-message-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-xml-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-txt-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-shader-mode.el")

;;; Do stuff after initialize.
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-after-init.el")

;;------------------------------------------------------------------------------------------------------
;; This is the end of .emacs file
