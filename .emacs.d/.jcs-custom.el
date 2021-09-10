(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(0xc actionscript-mode adaptive-wrap alt-codes apache-mode applescript-mode atl-long-lines atl-markup auth-source-keytar auto-highlight-symbol auto-read-only auto-rename-tag basic-mode better-scroll browse-kill-ring buffer-move buffer-wrap cask cask-mode ccls centaur-tabs clojure-mode cmake-font-lock cobol-mode com-css-sort command-log-mode company-box company-c-headers company-emojify company-fuzzy counsel csharp-mode csproj-mode dashboard-ls define-it diff-hl diminish diminish-buffer dockerfile-mode docstr dumb-jump editorconfig el-mock elisp-def elisp-demos elixir-mode elm-mode emmet-mode emoji-github erlang ert-runner eshell-syntax-highlighting ess esup exec-path-from-shell expand-region feebleline ffmpeg-player file-header fill-page flx flycheck-grammarly flycheck-languagetool flycheck-popup-tip flycheck-pos-tip fountain-mode fsharp-mode gdscript-mode gitattributes-mode gitconfig-mode github-browse-file gitignore-mode gitignore-templates glsl-mode go-mode google-this goto-char-preview goto-line-preview haxe-mode helpful highlight-escape-sequences highlight-indent-guides highlight-numbers hl-todo htmltagwrap ialign iedit impatient-showdown indent-control ini-mode isearch-project ivy-file-preview ivy-searcher javadoc-lookup jayces-mode jenkinsfile-mode json-mode keypression kotlin-mode license-templates line-reminder logms logview lsp-dart lsp-docker lsp-grammarly lsp-haskell lsp-java lsp-latex lsp-ltex lsp-mssql lsp-origami lsp-pascal lsp-pyright lsp-sourcekit lsp-ui lua-mode manage-minor-mode-table markdown-toc masm-mode most-used-words move-text multi-shell multiple-cursors nasm-mode nhexl-mode nix-mode org-bullets organize-imports-java package-lint page-break-lines parse-it powershell processing-mode project project-abbrev python-mode quelpa-use-package rainbow-mode region-occurrences-highlighter restart-emacs reveal-in-folder right-click-context rjsx-mode rust-mode scala-mode scrollable-quick-peek scss-mode shader-mode show-eol smex sql-indent swift-mode togetherly transwin tree-sitter-indent tree-sitter-langs turbo-log typescript-mode undercover undo-tree use-ttf vimrc-mode visual-regexp vs-dark-theme vs-light-theme vue-mode web-mode which-key xref yaml-mode yascroll yasnippet-snippets)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-default ((t (:background "#1D1D1D"))))
 '(centaur-tabs-modified-marker-selected ((t (:background "#31343E" :foreground "white"))))
 '(centaur-tabs-modified-marker-unselected ((t (:background "#3D3C3D" :foreground "grey50"))))
 '(centaur-tabs-selected ((t (:background "#31343E" :foreground "white"))))
 '(centaur-tabs-selected-modified ((t (:background "#31343E" :foreground "white"))))
 '(centaur-tabs-unselected ((t (:background "#3D3C3D" :foreground "grey50"))))
 '(centaur-tabs-unselected-modified ((t (:background "#3D3C3D" :foreground "grey50"))))
 '(company-fuzzy-annotation-face ((t (:foreground "#7BABCA"))))
 '(company-preview ((t (:foreground "dark gray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "dark gray"))))
 '(company-scrollbar-fg ((t (:background "black"))))
 '(company-tooltip ((t (:background "light gray" :foreground "black"))))
 '(company-tooltip-annotation ((t (:foreground "#96A2AA"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:background "light gray" :foreground "#C00000"))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:background "steel blue" :foreground "#C00000"))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white"))))
 '(popup-tip-face ((t (:background "#424245" :foreground "#F1F1F1"))))
 '(rjsx-attr ((t (:foreground "#EEDD82"))))
 '(rjsx-tag ((t (:foreground "#87CEFA"))))
 '(rjsx-tag-bracket-face ((t (:inherit 'web-mode-html-attr-name-face))))
 '(rjsx-text ((t (:inherit default)))))

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)  ; Enable downcase-region
(put 'upcase-region 'disabled nil)    ; Enable upcase-region
