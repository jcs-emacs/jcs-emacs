(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ivy flycheck csproj-mode lsp-mode reload-emacs multi-shell jayces-mode file-header yasnippet-snippets yasnippet yascroll yaml-mode which-key wgrep-ag web-mode impatient-mode vue-mode vs-light-theme vs-dark-theme visual-regexp vimrc-mode use-ttf use-package undo-tree typescript-mode togetherly swift-mode sql-indent smex scss-mode ssass-mode show-eol shader-mode scala-mode rust-mode rjsx-mode right-click-context reveal-in-folder restart-emacs region-occurrences-highlighter rainbow-mode quelpa python-mode project-abbrev processing-mode preproc-font-lock parse-it package-lint package-build organize-imports-java org-bullets nhexl-mode neotree nasm-mode multiple-cursors move-text masm-mode markdown-toc manage-minor-mode-table manage-minor-mode magit lua-mode lsp-ui lsp-origami lsp-java line-reminder kotlin-mode json-mode js2-mode javadoc-lookup isearch-project ini-mode indicators iedit htmltagwrap hl-todo highlight-indent-guides haxe-mode haskell-mode goto-line-preview goto-char-preview google-this go-mode glsl-mode gitignore-mode gitconfig-mode gitattributes-mode gdscript-mode flycheck-pos-tip flycheck-popup-tip flycheck-grammarly flx ffmpeg-player feebleline exec-path-from-shell esup ess erlang emoji-github emmet-mode elixir-mode elisp-def dumb-jump dockerfile-mode diminish-buffer diminish define-it dashboard-ls dashboard dart-mode dap-mode csharp-mode counsel-projectile counsel company-quickhelp-terminal company-quickhelp company-lsp company-fuzzy company-emoji company command-log-mode com-css-sort cobol-mode cmake-font-lock clojure-mode centaur-tabs browse-kill-ring buffer-wrap buffer-move basic-mode auto-rename-tag auto-highlight-symbol apache-mode alt-codes ag adaptive-wrap actionscript-mode ace-window)))

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
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
