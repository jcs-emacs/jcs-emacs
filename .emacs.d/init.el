;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Author:  Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL:     https://github.com/jcs090218/jcs-emacs

;;
;;                      ════╦╦╦╦╗
;;                  ╔═══════╩╩╩╩╩═════╗
;;                ══╝ ╔═════════════╗ ║
;;          ════════════════════════╬═╬══╗
;;           \   ╔══╗ ╓  ╥ ╥  ╥     ║ ╚══╬════
;;            ║  ║ ═╦ ║\ ║ ║  ║     ╚════╬════
;;            ║  ╚══╝ ╨ `╜ ╚══╝          ║
;;            ║   ╔══ ╔╗╔╗ ╔═╗ ╔═╕ ╔═╕   ║
;;            ║   ╠═  ║╙╜║ ╟─╢ ║   ╚═╗   ║
;;            ║   ╚══ ╨  ╨ ╨ ╨ ╚═╛ ╘═╝   ║
;;             \            _  _         /
;;              \═══════════╣  ╠════════/
;;                 ═════════╝  ║
;;                 ════════════╝
;;
;;                [J C S - E M A C S]
;;

;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;;
;; I barely know how to program LISP, and I know even less about ELISP.
;; So take everything in this file with a grain of salt!

;;; Code:

;;
;; (@* "Startup" )
;;

(defconst jcs-min-require-version "27.1"
  "Minimum required Emacs version for `JCS` configuration.")

(when (version< emacs-version jcs-min-require-version)
  (error (format "This requires Emacs %s and above!" jcs-min-require-version)))

(defconst jcs-gc-cons-threshold (* 1024 1024 20)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defconst jcs-gc-cons-upper-limit (* 1024 1024 128)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defun jcs-gc-cons-threshold-speed-up (speedup)
  "Raise/Lower GC threshold by SPEEDUP."
  (setq gc-cons-threshold (if speedup jcs-gc-cons-upper-limit jcs-gc-cons-threshold)))

(jcs-gc-cons-threshold-speed-up t)  ; Raise GC when starting Emacs!

(defconst jcs-file-name-handler-alist file-name-handler-alist
  "Record file name handler alist.")

;;
;; (@* "Optimizations" )
;;

(setq file-name-handler-alist nil)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)

;;
;; (@* "Version" )
;;

(defconst jcs-emacs-version-number "7.0.1"
  "JCS-Emacs version.")

(defun jcs-emacs-version ()
  "Show JCS-Emacs version info."
  (interactive)
  (message "JCS-Emacs %s" jcs-emacs-version-number))

;;
;; (@* "Load Core" )
;;

(add-to-list 'load-path "~/.emacs.jcs/")
(add-to-list 'load-path "~/.emacs.jcs/func/")
(add-to-list 'load-path "~/.emacs.jcs/mode/")

;;; Initialize
(require 'jcs-package)
(jcs-package-install-all)

;;; Utilities
(require 'jcs-log)
(require 'jcs-face)
(require 'jcs-function)

;;; Environment
(require 'jcs-file)
(require 'jcs-dev)
(require 'jcs-env)
(require 'jcs-theme)
(require 'jcs-plugin)

;;; Standardize
(require 'jcs-template)
(require 'jcs-mode)
(require 'jcs-project)

;;; Finalize
(require 'jcs-minibuf)
(require 'jcs-hook)
(require 'jcs-key)

;;; Customize
(require 'jcs-config)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(0xc actionscript-mode ada-mode adaptive-wrap alt-codes apache-mode applescript-mode asoc auth-source-keytar auto-highlight-symbol auto-read-only auto-rename-tag basic-mode better-scroll browse-kill-ring buffer-move buffer-wrap cask cask-mode ccls centaur-tabs clojure-mode cmake-font-lock cobol-mode com-css-sort command-log-mode company-box company-c-headers company-emojify company-fuzzy company-meta-net company-nginx counsel csproj-mode csv-mode dashboard-ls define-it diff-hl diminish diminish-buffer dockerfile-mode docstr dumb-jump editorconfig el-mock eldoc-meta-net elisp-def elisp-demos elixir-mode elm-mode emmet-mode emoji-github erlang ert-runner eshell-syntax-highlighting ess esup exec-path-from-shell expand-region file-header fill-page flx flx-rs flycheck-grammarly flycheck-languagetool flycheck-popup-tip flycheck-pos-tip fountain-mode fsharp-mode gdscript-mode git-modes github-browse-file github-tags gitignore-templates glsl-mode go-mode google-this goto-char-preview goto-line-preview haxe-mode helpful highlight-escape-sequences highlight-indent-guides highlight-numbers hl-preproc hl-todo htmltagwrap ialign iedit impatient-showdown indent-control ini-mode isearch-project ivy-file-preview ivy-searcher javadoc-lookup jayces-mode jenkinsfile-mode json-mode keypression kotlin-mode leaf license-templates line-reminder logms logview lsp-dart lsp-docker lsp-grammarly lsp-haskell lsp-java lsp-latex lsp-ltex lsp-metals lsp-mssql lsp-pascal lsp-pyright lsp-sonarlint lsp-sourcekit lsp-tailwindcss lsp-ui lua-mode manage-minor-mode-table markdown-toc masm-mode meta-view most-used-words move-text multi-shell multiple-cursors nasm-mode nginx-mode nhexl-mode nix-mode org-bullets organize-imports-java package-lint page-break-lines parse-it powershell processing-mode project project-abbrev python-mode rainbow-mode region-occurrences-highlighter restart-emacs reveal-in-folder right-click-context rjsx-mode rust-mode scrollable-quick-peek scss-mode shader-mode show-eol smex sort-words sql-indent swift-mode togetherly transwin tree-sitter-indent tree-sitter-langs ts ts-fold turbo-log typescript-mode undercover undo-tree use-ttf vimrc-mode visual-regexp vs-dark-theme vs-light-theme vue-mode web-mode which-key xref yaml-mode yascroll yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-fuzzy-annotation-face ((t (:foreground "#7BABCA"))))
 '(company-preview ((t (:foreground "dark gray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "#252526" :foreground "#BEBEBF"))))
 '(company-tooltip-annotation ((t (:foreground "#96A2AA"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:background "#252526" :foreground "#0096FA"))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:background "#062F4A" :foreground "#0096FA"))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#686868"))))
 '(company-tooltip-scrollbar-track ((t (:background "#3E3E42"))))
 '(company-tooltip-selection ((t (:background "#062F4A" :foreground "#BEBEBF"))))
 '(popup-tip-face ((t (:background "#2A2D38" :foreground "#F1F1F1")))))
 ;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
