;;; .emacs --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;;
;; I barely know how to program LISP, and I know
;; even less about ELISP.  So take everything in
;; this file with a grain of salt!
;;
;; - JenChieh

;;; Code:


;; DEBUG(jenchieh): Debug mode?
;; Produce backtraces when errors occur.
(setq debug-on-error t)


(defvar jcs-init-gc-cons-threshold (* 1024 1024 128)
  "The `GC' threshold during starting up.")
(defvar jcs-normal-gc-cons-threshold (* 1024 1024 20)
  "The `GC' threshold during the normal task.")

;; NOTE(jenchieh): Raise the `GC' threshold when starting Emacs.
(setq gc-cons-threshold jcs-init-gc-cons-threshold)


;;------------------------------------------------------------------------------------------------------
;; Auto generated by Emacs.
;;------------------------------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-idle-interval 0.3)
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(delete-auto-save-files nil)
 '(delete-old-versions (quote other))
 '(flymake-google-cpplint-command "C:/jcs_ide_packages/jcs_win7_packages/cpplint/cpplint.exe")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(httpd-port 8877)
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(jdee-jdk-registry
   (quote
    (("1.8.0_111" . "C:/Program Files/Java/jdk1.8.0_111"))))
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function (quote ignore))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (15)))
 '(package-selected-packages
   (quote
    (skewer-mode elixir-mode erlang multiple-cursors helm-projectile projectile buffer-move beacon dashboard flycheck-popup-tip glsl-mode company-quickhelp origami line-reminder polymode yasnippet-snippets dash git-commit move-text cmake-font-lock restart-emacs web-server indicators focus dimmer goto-line-preview transient magit with-editor right-click-context ini-mode htmltagwrap auto-rename-tag indent-info cobol-mode swift-mode rust-mode yaml-mode hydra markdown-mode nhexl-mode sr-speedbar clojure-mode undo-tree tabbar javadoc-lookup typescript-mode haxe-mode yasnippet xcscope wgrep-helm wgrep-ag wgrep websocket vimrc-mode tablist sql-indent simple-httpd scss-mode s request-deferred request popup pkg-info pcache json-snatcher json-reformat htmlize highlight-indentation highlight helm-core helm-ag google-translate google-this google-maps fringe-helper flycheck f epl emmet-mode diminish deferred cmake-mode bind-key avy ace-window js2-mode helm company apache-mode xwidgete which-key web-mode visual-regexp use-ttf use-package tree-mode togetherly sublimity ssass-mode shader-mode scala-mode rainbow-mode python-mode project-abbrev processing-mode preproc-font-lock powerline pdf-tools package-lint package-build organize-imports-java nasm-mode lua-mode json-mode impatient-mode iedit helm-gtags haskell-mode go-mode gitignore-mode gitconfig-mode gitattributes-mode floobits exec-path-from-shell csharp-mode cpputils-cmake com-css-sort basic-mode auto-package-update auto-highlight-symbol ag adaptive-wrap actionscript-mode)))
 '(paradox-github-token t)
 '(send-mail-function (quote mailclient-send-it))
 '(version-control nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:foreground nil :background "#113D6F"))))
 '(ahs-face ((t (:foreground nil :background "#113D6F"))))
 '(ahs-plugin-defalt-face ((t (:foreground nil :background "#123E70"))))
 '(company-preview ((t (:foreground "dark gray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "dark gray"))))
 '(company-scrollbar-fg ((t (:background "black"))))
 '(company-tooltip ((t (:background "light gray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white"))))
 '(css-selector ((t (:inherit font-lock-function-name-face :foreground "#17A0FB"))))
 '(dashboard-banner-logo-title ((t (:foreground "cyan1"))))
 '(dashboard-heading ((t (:foreground "#17A0FB"))))
 '(widget-button ((t (:foreground "light steel blue")))))

(put 'erase-buffer 'disabled nil)

;;------------------------------------------------------------------------------------------------------
;;;
;; Auto install list of packages I want at the startup of Emacs.
;;;

;; Get the list of package dependencies.
(load-file "~/.emacs.jcs/jcs-package.el")

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install all packages that this config needs.
(jcs-ensure-package-installed jcs-package-install-list)

;; activate installed packages
(package-initialize)

;;========================================
;;         Manually Installation
;;----------------------------------

(load-file "~/.emacs.d/elisp/jayces-mode-20190205.001/jayces-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex-pkg-20190326.001/jcs-ex-pkg.el")
(load-file "~/.emacs.d/elisp/reload-emacs-20190326.001/reload-emacs.el")


;;========================================
;;      JENCHIEH FILE LOADING
;;----------------------------------

(require 'reload-emacs)
(setq reload-emacs-load-path '("~/.emacs.jcs/"
                               "~/.emacs.jcs/func/"
                               "~/.emacs.jcs/mode/"))

;; NOTE(jenchieh): Add load path.
(unless reload-emacs-reloading
  (add-to-list 'load-path "~/.emacs.jcs/")
  (add-to-list 'load-path "~/.emacs.jcs/func/")
  (add-to-list 'load-path "~/.emacs.jcs/mode/"))


;; Environment.
(require 'jcs-face)
(require 'jcs-dev)
(require 'jcs-env)
(require 'jcs-plugin)

;; Customization
(require 'jcs-theme)

;; Initialize
(require 'jcs-before-init)

;; Utilities
(require 'jcs-log)
(require 'jcs-function)
(require 'jcs-corresponding-file)
(require 'jcs-file-info-format)
(require 'jcs-mode)

;; Finalize
(require 'jcs-hook)
(require 'jcs-font)
(require 'jcs-after-init)


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; .emacs ends here
