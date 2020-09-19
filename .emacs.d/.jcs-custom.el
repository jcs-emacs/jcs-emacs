(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(show-eol yasnippet-snippets yascroll yaml-mode which-key web-mode vue-mode vs-light-theme vs-dark-theme visual-regexp vimrc-mode use-ttf use-package undo-tree typescript-mode transwin togetherly test-sha swift-mode sql-indent smex shader-mode scss-mode scala-mode rust-mode rjsx-mode right-click-context reveal-in-folder restart-emacs reload-emacs region-occurrences-highlighter rainbow-mode quelpa python-mode project-abbrev processing-mode preproc-font-lock powershell parse-it package-lint package-build organize-imports-java org-bullets nix-mode nhexl-mode neotree nasm-mode multiple-cursors multi-shell move-text most-used-words masm-mode markdown-toc manage-minor-mode-table magit lua-mode lsp-ui lsp-origami lsp-java line-reminder license-templates kotlin-mode keypression json-mode jayces-mode javadoc-lookup ivy-searcher ivy-file-preview isearch-project ini-mode impatient-showdown iedit ialign htmltagwrap hl-todo highlight-indent-guides helpful haxe-mode haskell-mode groovy-mode goto-line-preview goto-char-preview google-this go-mode glsl-mode gitignore-templates gitignore-mode github-browse-file gitconfig-mode gitattributes-mode gdscript-mode fountain-mode flycheck-pos-tip flycheck-popup-tip flycheck-grammarly flx file-header ffmpeg-player feebleline exec-path-from-shell esup ess eshell-syntax-highlighting erlang emoji-github emmet-mode elixir-mode elisp-demos elisp-def dumb-jump dockerfile-mode diminish-buffer diminish define-it dashboard-ls dart-mode csproj-mode csharp-mode counsel-projectile company-quickhelp-terminal company-fuzzy company-emoji command-log-mode com-css-sort cobol-mode cmake-font-lock clojure-mode centaur-tabs buffer-wrap buffer-move browse-kill-ring better-scroll basic-mode auto-rename-tag auto-read-only auto-highlight-symbol atl-markup apache-mode alt-codes adaptive-wrap actionscript-mode)))

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
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:background "light gray" :foreground "#C00000"))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:background "steel blue" :foreground "#C00000"))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white")))))

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)  ; Enable downcase-region
(put 'upcase-region 'disabled nil)    ; Enable upcase-region
