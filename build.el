;; ========================================================================
;; $File: build.el $
;; $Date: 2018-11-25 20:35:16 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


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
    (ac-emmet cobol-mode swift-mode rust-mode yaml-mode hydra markdown-mode markdown-preview-mode nhexl-mode sr-speedbar clojure-mode undo-tree ein tabbar javadoc-lookup typescript-mode haxe-mode yasnippet xcscope wgrep-helm wgrep-ag wgrep websocket vimrc-mode tablist sql-indent skewer-mode simple-httpd scss-mode s request-deferred request popup pkg-info php-mode php-auto-yasnippets pcache multiple-cursors memoize levenshtein json-snatcher json-reformat htmlize highlight-indentation highlight helm-core helm-ag google-translate google-this google-maps git-messenger git-commit fringe-helper flymake-easy flycheck find-file-in-project f epl emmet-mode diminish deferred cmake-mode bind-key avy auto-complete async ace-window ac-php-core with-editor pyvenv magit-popup js2-mode ghub dash ivy helm company apache-mode xwidgete which-key web-mode visual-regexp use-ttf use-package tree-mode togetherly sublimity ssass-mode shader-mode scala-mode rainbow-mode python-mode py-autopep8 project-abbrev processing-mode preproc-font-lock powerline pdf-tools package-lint package-build organize-imports-java nasm-mode multi-web-mode meghanada magit lua-mode line-reminder json-mode js2-refactor jdee impatient-mode iedit helm-gtags haskell-mode google-c-style go-mode gitlab gitignore-mode github-notifier gitconfig-mode gitattributes-mode git-timemachine git-link flymake-google-cpplint flymake-cursor floobits exec-path-from-shell elpy csharp-mode cpputils-cmake com-css-sort cmake-project cmake-ide cmake-font-lock better-defaults basic-mode auto-package-update auto-highlight-symbol auto-complete-c-headers all-the-icons ag adaptive-wrap actionscript-mode ac-php ac-js2 ac-html)))
 '(send-mail-function (quote mailclient-send-it))
 '(version-control nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:foreground nil :background "#113D6F"))))
 '(ahs-face ((t (:foreground nil :background "#113D6F"))))
 '(ahs-plugin-defalt-face ((t (:foreground nil :background "#123E70")))))


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


(load-file "./.emacs.jcs/jcs-package.el")

;; Install all needed packages without asking.
(jcs-ensure-package-installed jcs-package-install-list t)


;; Start regular Emacs file.
;;(load-file "./.emacs")
