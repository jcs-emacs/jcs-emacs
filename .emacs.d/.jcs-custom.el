(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(delete-auto-save-files nil)
 '(delete-old-versions (quote other))
 '(flymake-google-cpplint-command "C:/jcs_ide_packages/jcs_win7_packages/cpplint/cpplint.exe")
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
    (helm go-mode helm-core feebleline line-reminder helm-projectile helm-file-preview helm-ag indicators centaur-tabs highlight-indent-guides use-ttf alt-codes preproc-font-lock gdscript-mode rainbow-mode haxe-mode dumb-jump ssass-mode region-occurrences-highlighter flycheck yascroll esup swift-mode flycheck-popup-tip dart-mode basic-mode beacon google-this right-click-context show-eol csharp-mode isearch-project async hl-todo page-break-lines goto-char-preview elixir-mode erlang multiple-cursors projectile buffer-move dashboard glsl-mode company-quickhelp origami yasnippet-snippets dash git-commit move-text cmake-font-lock restart-emacs focus dimmer goto-line-preview transient magit with-editor ini-mode htmltagwrap auto-rename-tag cobol-mode rust-mode yaml-mode markdown-mode nhexl-mode sr-speedbar clojure-mode undo-tree javadoc-lookup typescript-mode yasnippet xcscope wgrep-helm wgrep-ag wgrep vimrc-mode sql-indent simple-httpd scss-mode s popup pkg-info json-snatcher json-reformat htmlize google-translate f epl emmet-mode diminish cmake-mode bind-key avy ace-window js2-mode company apache-mode which-key web-mode use-package tree-mode togetherly sublimity shader-mode scala-mode python-mode project-abbrev processing-mode powerline package-lint package-build organize-imports-java nasm-mode lua-mode json-mode impatient-mode iedit haskell-mode gitignore-mode gitconfig-mode gitattributes-mode exec-path-from-shell com-css-sort auto-highlight-symbol ag adaptive-wrap actionscript-mode)))
 '(paradox-github-token t)
 '(send-mail-function (quote mailclient-send-it))
 '(version-control nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "dark gray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "dark gray"))))
 '(company-scrollbar-fg ((t (:background "black"))))
 '(company-tooltip ((t (:background "light gray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white")))))

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
