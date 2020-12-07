;;; jcs-plugin.el --- Plugin Configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package adaptive-wrap
  :defer t
  :init
  (add-hook 'visual-line-mode-hook (lambda () (adaptive-wrap-prefix-mode +1))))

(use-package atl-markup
  :defer t
  :init
  (setq atl-markup-delay 0.0))

(use-package auto-highlight-symbol
  :defer t
  :init
  (setq ahs-idle-interval 0.3))

(use-package auto-read-only
  :defer t
  :config
  (add-to-list 'auto-read-only-file-regexps "/[.]emacs[.]d/elisp/")
  (add-to-list 'auto-read-only-file-regexps "/[.]emacs[.]d/elpa/")
  (add-to-list 'auto-read-only-file-regexps "/lisp/")

  (defun jcs--auto-read-only--hook-find-file ()
    "Advice wrap `auto-read-only--hook-find-file' function."
    (when (and (not jcs-package-installing-p)
               (not (jcs-project-current)))
      (auto-read-only)))
  (advice-add 'auto-read-only--hook-find-file :override #'jcs--auto-read-only--hook-find-file))

(use-package auto-rename-tag
  :defer t
  :init
  (setq auto-rename-tag-disabled-commands '(query-replace)
        auto-rename-tag-disabled-minor-modes '(iedit-mode
                                               multiple-cursors-mode)))

(use-package better-scroll
  :defer t
  :init
  (setq better-scroll-align-type 'relative
        better-scroll-allow-boundary-movement t))

(use-package browse-kill-ring
  :defer t
  :init
  (setq browse-kill-ring-separator (jcs-env-separator)
        browse-kill-ring-separator-face 'font-lock-comment-face)
  :config
  (defun jcs--browse-kill-ring-mode-hook ()
    "Hook for `browse-kill-ring-mode'."
    (setq browse-kill-ring-separator (jcs-env-separator))
    (page-break-lines-mode 1))
  (add-hook 'browse-kill-ring-mode-hook 'jcs--browse-kill-ring-mode-hook))

(use-package buffer-wrap
  :defer t
  :config
  (defun jcs--buffer-wrap--fixed-window-off ()
    "Fixed windows is off after wrapping."
    (let ((max-ln (+ (line-number-at-pos (point-max)) buffer-wrap--relative-max-line)))
      (when (= max-ln (line-number-at-pos (point)))
        (jcs-recenter-top-bottom 'bottom))))

  (defun jcs--buffer-wrap--fixed-fake-header ()
    "Fixed line offset consider fake header calculation."
    (when tabulated-list-format
      (unless (ignore-errors (tabulated-list-get-entry))
        (cond ((= 0 buffer-wrap--delta-lines)
               (goto-char (point-min)))
              ((< 0 buffer-wrap--delta-lines)
               (ignore-errors (forward-line 1)))
              (t
               (jcs-goto-line (1- (line-number-at-pos (point-max))))))
        (unless (ignore-errors (tabulated-list-get-entry))
          (ignore-errors (forward-line 1))))))

  (defun jcs--buffer-wrap-post-command-hook ()
    "Buffer Wrap post command hook."
    (jcs--buffer-wrap--fixed-fake-header)
    (jcs--buffer-wrap--fixed-window-off))
  (add-hook 'buffer-wrap-post-command-hook 'jcs--buffer-wrap-post-command-hook))

(use-package centaur-tabs
  :defer t
  :init
  (setq centaur-tabs-set-icons nil
        centaur-tabs-style "wave"
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"))

(use-package company
  :defer t
  :init
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-require-match nil
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-eclim-auto-save nil)
  ;; TOPIC: How add company-dabbrev to the Company completion popup?
  ;; URL: https://emacs.stackexchange.com/questions/15246/how-add-company-dabbrev-to-the-company-completion-popup
  (setq company-backends
        (append
         ;; --- Internal ---
         '(company-capf company-semantic)
         '(company-keywords)
         '(company-abbrev company-dabbrev company-dabbrev-code)
         '(company-files)
         '(company-etags company-gtags)
         '(company-yasnippet)
         ;; --- External ---
         '(company-emoji)))
  (setq company-minimum-prefix-length 0
        company-idle-delay 0.1
        company-selection-wrap-around 'on)
  :config
  (with-eval-after-load 'company (require 'jcs-company) (global-company-mode t)))

(use-package company-c-headers
  :defer t
  :config
  (require 'dash)

  (defconst jcs--msvc-path
    '("C:/Program Files (x86)/Microsoft Visual Studio/"
      "/Community/VC/Tools/MSVC/"
      "/include/")
    "Path for Microsoft Visual Studio.")

  (defconst jcs--windows-kits-path
    '("C:/Program Files (x86)/Windows Kits/10/Include/"
      "/ucrt/")
    "Path for Windows Kits.")

  (setq company-c-headers-path-user '(".")
        company-c-headers-path-system
        (-flatten
         (append
          '("/usr/include/" "/usr/local/include/")
          (list (jcs--path-guess jcs--msvc-path
                                 (lambda (dirname)
                                   (not (= (string-to-number dirname) 0))))
                (jcs--path-guess jcs--windows-kits-path
                                 (lambda (dirname)
                                   (not (= (string-to-number dirname) 0)))))))))

(use-package company-emoji
  :defer t
  :init
  (setq emojify-company-tooltips-p t))

(use-package company-fuzzy
  :defer t
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-history-backends '(company-yasnippet)
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'"))
  (with-eval-after-load 'company (global-company-fuzzy-mode t)))

(use-package company-quickhelp
  :defer t
  :init
  (setq company-quickhelp-delay 0.3
        company-quickhelp-color-background "#FFF08A")
  (with-eval-after-load 'company (company-quickhelp-mode t)))

(use-package company-quickhelp-terminal
  :defer t
  :init
  (with-eval-after-load 'company-quickhelp
    (unless (display-graphic-p) (company-quickhelp-terminal-mode 1))))

(use-package counsel
  :defer t
  :init
  (setq counsel-preselect-current-file t
        counsel-find-file-at-point t))

(use-package csharp-mode
  :defer t
  :init
  (setq csharp-codedoc-tag-face 'font-lock-doc-face))

(use-package dashboard
  :defer t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name))
        dashboard-banner-logo-title "[J C S • E M A C S]"
        dashboard-footer-icon ""
        dashboard-footer-messages
        `(,(format "╬ Copyright (c) %s Shen, Jen-Chieh ╬" (jcs-get-year-only)))
        dashboard-init-info (format "%d + %d packages loaded in %0.1f seconds"
                                    (length package-activated-list)
                                    (length jcs-package-manually-install-list)
                                    (string-to-number jcs-package-init-time))
        dashboard-items '((recents  . 10)
                          (projects . 10)
                          ;;(bookmarks . 10)
                          ;;(agenda . 10)
                          ;;(registers . 10)
                          )
        dashboard-item-shortcuts '((recents . "r")
                                   (bookmarks . "m")
                                   (projects . "p")
                                   (agenda . "a")
                                   (registers . "e")
                                   (ls-directories . "d")
                                   (ls-files . "f"))
        dashboard-center-content t
        dashboard-set-navigator nil
        dashboard-projects-backend 'projectile)
  :config
  (require 'dashboard-ls)
  (defun jcs--dashboard-insert-page-break--advice-before (&rest _)
    "Re-new page separator."
    (setq dashboard-page-separator (format "\n%s\n" (jcs-env-separator))))
  (advice-add #'dashboard-insert-page-break :before #'jcs--dashboard-insert-page-break--advice-before)

  (dashboard-setup-startup-hook))

(use-package dashboard-ls
  :defer t
  :config
  (let ((dashboard-lst-items '((ls-directories . 5)
                               (ls-files . 5))))
    (setq dashboard-items (append dashboard-lst-items dashboard-items))))

(use-package define-it
  :defer t
  :init
  (setq define-it-output-choice 'view
        define-it-define-word-header "--{{ DEFINE }}--\n\n"
        define-it-definition-header "\n\n--{{ DICTIONARY }}--\n\n"
        define-it-translate-header "\n\n--{{ TRANSLATION }}--\n\n"
        define-it-wiki-summary-header "\n\n--{{ WIKIPEDIA SUMMARY }}--\n\n"))

(use-package diminish
  :defer t
  :config
  (defun jcs-diminish-type (type)
    "Diminsh TYPE.

  Argument TYPE can either be a list or a symbol."
    (cond ((listp type) (dolist (sym type) (diminish sym)))
          ((symbolp type) (diminish type))
          (t (user-error "Invalid diminish symbol, %s" type))))

  (defun jcs-diminish (mode-sym &optional load-sym)
    "Diminish MODE-SYM.

  If argument LOAD-SYM is a symbol; then it will diminish after it's module
  is loaded using macro `with-eval-after-load'."
    (if load-sym (with-eval-after-load load-sym(jcs-diminish-type mode-sym))
      (jcs-diminish-type mode-sym)))

  (defun jcs-diminish-do-alist (alst)
    "Diminish the whole ALST."
    (dolist (type alst) (jcs-diminish (car type) (cdr type))))

  (defvar jcs-diminish-alist
    `((abbrev-mode)
      (alt-codes-mode . alt-codes)
      (auto-fill-mode)
      (auto-fill-function)
      (auto-highlight-symbol-mode . auto-highlight-symbol)
      (auto-read-only-mode . auto-read-only)
      (auto-rename-tag-mode . auto-rename-tag)
      (atl-markup-mode . atl-markup)
      (auto-revert-mode . autorevert)
      (buffer-wrap-mode . buffer-wrap)
      (command-log-mode . command-log-mode)
      (company-mode . company)
      (company-fuzzy-mode . company-fuzzy)
      (docstr-mode . docstr)
      (eldoc-mode)
      (emmet-mode . emmet-mode)
      (buffer-face-mode . face-remap)
      (fill-page-mode . fill-page)
      (flycheck-mode . flycheck)
      (helm-mode . helm-mode)
      (hi-lock-mode . hi-lock)
      (highlight-indent-guides-mode . highlight-indent-guides)
      (impatient-mode . impatient-mode)
      (ivy-mode . ivy)
      (keypression-mode . keypression)
      (line-reminder-mode . line-reminder)
      (indicators-mode . indicators)
      (outline-minor-mode)
      (overwrite-mode)
      (page-break-lines-mode . page-break-lines)
      (projectile-mode . projectile)
      (right-click-context-mode . right-click-context)
      (shift-select-minor-mode . shift-select)
      (show-eol-mode . show-eol)
      (un-mini-mode . un-mini)
      (undo-tree-mode . undo-tree)
      (view-mode . view)
      (visual-line-mode)
      (which-key-mode . which-key)
      ((whitespace-mode whitespace-newline-mode) . whitespace)
      (yas-minor-mode . yasnippet))
    "List of diminish associated list.")

  (jcs-diminish-do-alist jcs-diminish-alist))

(use-package diminish-buffer
  :defer t
  :init
  (setq diminish-buffer-list
        (append
         '("[*]helm" "[*]esup-" "[*]quelpa-")
         '("[*]compilation" "[*]output")
         '("[*]Apropos[*]" "[*]Backtrace[*]" "[*]Compile-Log[*]" "[*]Help[*]"
           "[*]Warnings[*]")
         '("[*]VC-history[*]")
         '("[*]CPU-Profiler-Report" "[*]Memory-Profiler-Report")
         '("[*]Process List[*]")
         '("[*]Checkdoc " "[*]Package-Lint[*]")
         '("[*]Async Shell Command[*]" "[*]shell" "[*]eshell")
         '("[*]ESS[*]")
         '("[*]emacs[*]")  ; From `async'
         '("[*]lsp-" "[*][a-zA-Z0-9]+[-]*ls" "[*][a-zA-Z0-9]+::stderr[*]"
           "[*]csharp[*]")  ; From `lsp'
         '("[*]company")
         '("[*]Local Variables[*]")
         '("[*]Kill Ring[*]")  ; From `browse-kill-ring'
         '("[*]SPEEDBAR")
         '("[*]Flycheck error" "[*]Flymake log[*]")
         '("[*]httpd[*]")
         '("[*]helpful")
         '("[*]Most used words[*]")
         '("[*]Test SHA[*]")
         '("[*]RE-Builder")
         '("[*]preview-it")
         '("[*]wclock[*]"))
        diminish-buffer-mode-list
        (append
         '("Dired by name")))
  (with-eval-after-load 'jcs-buffer-menu (diminish-buffer-mode 1))
  :config
  (defun jcs--diminish-buffer-clean--advice-before ()
    "Advice do clean buffer."
    (when diminish-buffer-mode (diminish-buffer-clean)))
  (advice-add 'jcs-buffer-menu-refresh-buffer :before #'jcs--diminish-buffer-clean--advice-before))

(use-package dumb-jump
  :defer t
  :init
  (setq dumb-jump-selector 'ivy))

(use-package elisp-demos
  :defer t
  :init
  (with-eval-after-load 'help-fns
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

(use-package eshell-syntax-highlighting
  :defer t
  :init
  (with-eval-after-load 'eshell
    (eshell-syntax-highlighting-global-mode +1)))

(use-package eww
  :defer t
  :init
  (setq eww-search-prefix "https://www.google.com/search?q="))

(use-package exec-path-from-shell
  :defer t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package feebleline
  :defer t
  :init
  (setq feebleline-msg-functions
        '(;;-- Left
          (jcs--feebleline--prepare)
          (jcs--feebleline--lsp-info)
          (jcs--feebleline--major-mode)
          (jcs--feebleline--buffer-name)
          (jcs--feebleline--project-name-&-vc-info)
          ;;-- Right
          (jcs--feebleline--symbol-read-only :align right)
          (jcs--feebleline--coding-system-&-line-endings :align right)
          (jcs--feebleline--spc/tab-&-width :align right)
          (jcs--feebleline--line/column :align right)
          (jcs--feebleline--time :align right)))

  (defun jcs-feebleline-display-mode-line-graphic ()
    "Display feebleline graphic base on the is inside terminal or not."
    (when feebleline-mode
      (jcs-walk-through-all-buffers-once
       (lambda ()
         (if (display-graphic-p)
             (setq mode-line-format nil)
           (jcs-feebleline-revert-terminal-mode-line)
           (setq mode-line-format (jcs--feebleline--mode-line-window-width-string)))))))

  (defun jcs-feebleline-revert-terminal-mode-line ()
    "Revert the terminal mode-line when using feebleline."
    (let* ((ml-color (jcs--get-mode-line-color))
           (ac-lst (car ml-color)) (inac-lst (cdr ml-color)))
      (jcs--set-mode-line-color--by-feebleline ac-lst inac-lst)))
  :config
  (cl-defun jcs--feebleline--insert-func (func &key (face 'default) pre (post " ") (fmt "%s") (align 'left))
    "Override `feebleline--insert-func' function."
    (list align
          (let* ((msg (apply func nil)) (str (concat pre (format fmt msg) post)))
            (if msg (if (equal face 'default) str (propertize str 'face face)) ""))))
  (advice-add 'feebleline--insert-func :override #'jcs--feebleline--insert-func)

  (defun jcs--feebleline--insert ()
    "Override `feebleline--insert' function."
    (unless (current-message)
      (let ((left-str ()) (right-str ()))
        (dolist (idx feebleline-msg-functions)
          (let* ((fragment (apply 'feebleline--insert-func idx))
                 (align (car fragment))
                 (string (cadr fragment)))
            (cl-case align
              (left (push string left-str))
              (right (push string right-str))
              (t (push string left-str)))))
        (with-current-buffer feebleline--minibuf
          (erase-buffer)
          (let* ((left-string (string-join (reverse left-str)))
                 (message-truncate-lines t)
                 (max-mini-window-height 1)
                 (right-string (string-join (reverse right-str)))
                 (free-space (- (jcs-max-frame-width) (length left-string) (length right-string)))
                 (padding (make-string (max 0 free-space) ?\ )))
            (insert (concat left-string (if right-string (concat padding right-string)))))))))
  (advice-add 'feebleline--insert :override #'jcs--feebleline--insert)

  (defconst jcs--feebleline--in-case-mode-line-width 10
    "Mode line width that add up to window width, in case it goes over window width.")

  (defun jcs--feebleline--mode-line-window-width ()
    "Get the mode line's format width by window width plus in case value."
    (+ (frame-width) jcs--feebleline--in-case-mode-line-width))

  (defun jcs--feebleline--mode-line-window-width-string ()
    "Get the mode line's format string by window width plus in case value."
    (jcs-fill-n-char-seq "_" (jcs--feebleline--mode-line-window-width)))

  (defun jcs--feebleline-mode--advice-after (&rest _)
    "Advice after execute `feebleline-mode'."
    (if feebleline-mode
        (jcs-feebleline-display-mode-line-graphic)
      (window-divider-mode -1)
      (jcs-walk-through-all-buffers-once
       (lambda ()
         (setq mode-line-format feebleline--mode-line-format-previous)))))
  (advice-add 'feebleline-mode :after #'jcs--feebleline-mode--advice-after))

(use-package ffmpeg-player
  :defer t
  :init
  (setq ffmpeg-player--volume 75
        ffmpeg-player-display-width 672
        ffmpeg-player-display-height 378
        ffmpeg-player-no-message t)
  :config
  (defun jcs--ffmpeg-player-before-insert-image-hook ()
    "Hook runs before inserting image."
    (insert "             "))
  (add-hook 'ffmpeg-player-before-insert-image-hook 'jcs--ffmpeg-player-before-insert-image-hook)

  (defun jcs-ffmpeg-player-mode-hook ()
    "Hook runs in `ffmpeg-player-mode'."
    (setq-local
     feebleline-msg-functions
     '(;;-- Left
       (jcs--feebleline--prepare)
       (jcs--feebleline--lsp-info)
       (jcs--feebleline--major-mode)
       (jcs--feebleline--buffer-name)
       (jcs--feebleline--project-name-&-vc-info)
       ;;-- Right
       (jcs--feebleline--symbol-read-only :align right)
       (jcs--feebleline--timeline :align right)
       (jcs--feebleline--pause-mute-volume :align right)
       (jcs--feebleline--time :align right))))
  (add-hook 'ffmpeg-player-mode-hook 'jcs-ffmpeg-player-mode-hook))

(use-package file-header
  :defer t
  :init
  (setq file-header-template-config-filepath "~/.emacs.jcs/template/template_config.properties"))

(use-package flycheck-grammarly
  :defer t
  :init
  (with-eval-after-load 'flycheck (require 'flycheck-grammarly)))

(use-package flycheck-popup-tip
  :defer t
  :init
  (defun jcs--flycheck-mode--pos-tip--advice-after (&rest _)
    "Advice runs after `flycheck-mode' function with `flycheck-popup-tip'."
    (jcs-enable-disable-mode-by-condition 'flycheck-popup-tip-mode
                                          (and (not (display-graphic-p)) flycheck-mode)))
  (advice-add 'flycheck-mode :after #'jcs--flycheck-mode--pos-tip--advice-after))

(use-package flycheck-pos-tip
  :defer t
  :init
  (defun jcs--flycheck-mode--pos-tip--advice-after (&rest _)
    "Advice runs after `flycheck-mode' function with `flycheck-pos-tip'."
    (jcs-enable-disable-mode-by-condition 'flycheck-pos-tip-mode
                                          (and (display-graphic-p) flycheck-mode)))
  (advice-add 'flycheck-mode :after #'jcs--flycheck-mode--pos-tip--advice-after))

(use-package google-translate
  :defer t
  :init
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh-TW"))

(use-package goto-char-preview
  :defer t
  :config
  (advice-add 'goto-char-preview :after #'jcs--recenter--advice-after))

(use-package goto-line-preview
  :defer t
  :config
  (advice-add 'goto-line-preview :after #'jcs--recenter--advice-after))

(use-package highlight-indent-guides
  :defer t
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-responsive 'top))

(use-package highlight-numbers
  :defer t
  :config
  (set-face-attribute 'highlight-numbers-number nil
                      :foreground "#9BCEA3"))

(use-package hl-todo
  :defer t
  :init
  (setq hl-todo-highlight-punctuation "")
  (setq hl-todo-keyword-faces
        '(("HOLD" . "#d0bf8f")
          ("TODO" . "red")
          ("NEXT" . "#dca3a3")
          ("THEM" . "#dc8cc3")
          ("PROG" . "#7cb8bb")
          ("OKAY" . "#7cb8bb")
          ("DONT" . "#5f7f5f")
          ("FAIL" . "#8c5353")
          ("DONE" . "#afd8af")
          ("NOTE"   . "dark green")
          ("KLUDGE" . "#d0bf8f")
          ("HACK"   . "#d0bf8f")
          ("TEMP"   . "turquoise")
          ("FIXME"  . "red")
          ("XXX+"   . "#cc9393")
          ("\\?\\?\\?+" . "#cc9393")

          ("ATTENTION" . "red")
          ("STUDY" . "yellow")
          ("IMPORTANT" . "yellow")
          ("CAUTION" . "yellow")
          ("OPTIMIZE" . "yellow")
          ("DESCRIPTION" . "dark green")
          ("TAG" . "dark green")
          ("OPTION" . "dark green")
          ("DEBUG" . "turquoise")
          ("DEBUGGING" . "turquoise")
          ("TEMPORARY" . "turquoise")
          ("SOURCE" . "PaleTurquoise2")
          ("URL" . "PaleTurquoise2")
          ("IDEA" . "green yellow")
          ("OBSOLETE" . "DarkOrange3")
          ("DEPRECATED" . "DarkOrange3")
          ("TOPIC" . "slate blue")
          ("SEE" . "slate blue")))
  :config
  (defun jcs--hl-todo--inside-comment-or-string-p ()
    "Redefine `hl-todo--inside-comment-or-string-p', for accurate highlighting."
    (jcs-inside-comment-or-string-p))
  (advice-add #'hl-todo--inside-comment-or-string-p :override #'jcs--hl-todo--inside-comment-or-string-p))

(use-package impatient-showdown
  :defer t
  :init
  (setq impatient-showdown-flavor 'github))

(use-package isearch
  :defer t
  :init
  (setq isearch-lazy-count t
        lazy-count-prefix-format "[%s:%s] "))

(use-package isearch-project
  :defer t
  :init
  (setq isearch-project-ignore-paths '(".vs/"
                                       ".vscode/"
                                       "bin/"
                                       "build/"
                                       "build.min/"
                                       "node_modules/"
                                       "res/"))
  :config
  (defun jcs-isearch-mode-hook ()
    "Paste the current symbol when `isearch' enabled."
    (cond ((use-region-p)
           (progn
             (deactivate-mark)
             (ignore-errors
               (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))))
          ((memq this-command '(jcs-isearch-project-backward-symbol-at-point))
           (when (char-or-string-p isearch-project--thing-at-point)
             (backward-word 1)
             (isearch-project--isearch-yank-string isearch-project--thing-at-point)
             (isearch-repeat-backward)))))
  (add-hook 'isearch-mode-hook #'jcs-isearch-mode-hook))

(use-package ivy
  :defer t
  :init
  (setq ivy-auto-shrink-minibuffer t
        ivy-use-virtual-buffers t  ; Enable bookmarks and recentf
        ivy-use-selectable-prompt t
        ivy-fixed-height-minibuffer t
        ivy-count-format "[%d:%d] "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-wrap t)
  (defvar jcs-ivy-height-ratio 0.3
    "Ratio that respect `frame-height' by multiply this and `ivy-height'.")
  :config
  (require 'smex)
  (setq enable-recursive-minibuffers t))

(use-package ivy-file-preview
  :defer t
  :init
  (setq ivy-file-preview-overlay-delay-time 0.2)
  (with-eval-after-load 'ivy (ivy-file-preview-mode 1)))

(use-package ivy-searcher
  :defer t
  :init
  (setq ivy-searcher-display-info 'line/column
        ivy-searcher-preselect 'next)
  :config
  (advice-add 'ivy-searcher-replace-file :after #'jcs-revert-all-file-buffers)
  (advice-add 'ivy-searcher-replace-project :after #'jcs-revert-all-file-buffers)
  (advice-add 'ivy-searcher-search-file :after #'jcs--recenter--advice-after)
  (advice-add 'ivy-searcher-search-project :after #'jcs--recenter--advice-after))

(use-package keypression
  :defer t
  :config
  (setq keypression-ignore-mouse-events
        (append keypression-ignore-mouse-events
                '(switch-frame menu-bar tool-bar tab-bar))))

(use-package line-reminder
  :defer t
  :init
  (setq line-reminder-show-option (if (display-graphic-p) 'indicators 'linum))
  (unless (display-graphic-p)
    (setq line-reminder-saved-sign " |"
          line-reminder-modified-sign " |")))

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-auto-guess-root t
        lsp-prefer-capf t
        lsp-keep-workspace-alive nil  ; Auto-kill LSP server
        lsp-prefer-flymake nil  ; Use lsp-ui and flycheck
        flymake-fringe-indicator-position 'right-fringe)

  (defconst jcs--lsp-lv-buffer-name " *LV*"
    "Help record the ` *LV*' buffer name.")

  (defvar-local jcs--lsp--executing-command nil
    "Flag to record if executing a command from `lsp'.")

  (defun jcs--lsp-connected-p ()
    "Check if LSP connected."
    (if (boundp 'lsp-managed-mode) lsp-managed-mode nil))

  (defun jcs--safe-lsp-active ()
    "Safe way to active LSP."
    (when (and (jcs-project-current)
               (ignore-errors (file-readable-p (buffer-file-name))))
      (unless (jcs--lsp-connected-p) (lsp-deferred))))

  (defun jcs--lsp-current-last-signature-buffer ()
    "Check if current buffer last signature buffer."
    (when (boundp 'lsp--last-signature-buffer)
      (let ((ls-buf (buffer-name lsp--last-signature-buffer)))
        (if (and (stringp ls-buf) (stringp (buffer-file-name)))
            (string-match-p ls-buf (buffer-file-name))
          nil))))

  (defun jcs--lsp-signature-maybe-stop ()
    "Maybe stop the signature action."
    (when (functionp 'lsp-signature-maybe-stop) (lsp-signature-maybe-stop))))

(use-package lsp-ui
  :defer t
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-delay 0.6
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border (face-foreground 'default)
        lsp-eldoc-enable-hover nil
        lsp-ui-imenu-enable t
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face))
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t)

  (defun jcs--lsp-ui-doc--hide-frame ()
    "Safe way to call `lsp-ui-doc--hide-frame' function."
    (when (and (functionp 'lsp-ui-doc--hide-frame) (not jcs--lsp-lv-recording))
      (lsp-ui-doc--hide-frame))))

(use-package most-used-words
  :defer t
  :init
  (setq most-used-words-display-type 'table
        most-used-words-word-display 100))

(use-package modablist
  :defer t
  :config
  (set-face-attribute 'modablist-select-face nil
                      :box '(:line-width -1 :color "#65A7E2" :style nil))
  (set-face-attribute 'modablist-insert-face nil :background "#565136"
                      :box '(:line-width -1 :color "#65A7E2" :style nil)))

(use-package multi-shell
  :defer t
  :init
  (setq multi-shell-prefer-shell-type 'shell))  ; Accept `shell' or `eshll'.

(use-package multiple-cursors
  :defer t
  :init
  (defconst jcs-mc/cancel-commands
    (append
     '(jcs-previous-blank-line jcs-next-blank-line)
     '(jcs-isearch-backward-symbol-at-point
       isearch-forward-symbol-at-point
       jcs-isearch-repeat-backward
       jcs-isearch-repeat-forward)
     '(jcs-isearch-project-backward-symbol-at-point
       isearch-project-forward-symbol-at-point
       jcs-isearch-project-repeat-backward
       jcs-isearch-project-repeat-forward))
    "List of commands that will quite `multiple-cursors' after execution.")

  (defun jcs-mc/cancel-multiple-cursors (&rest _)
    "Cancel the `multiple-cursors' behaviour."
    (when (and (functionp 'mc/num-cursors) (> (mc/num-cursors) 1))
      (mc/keyboard-quit)))

  (dolist (cmd jcs-mc/cancel-commands)
    (advice-add cmd :after #'jcs-mc/cancel-multiple-cursors))
  :config
  (defun jcs--mc/mark-lines (num-lines direction)
    "Override `mc/mark-lines' function."
    (let ((cur-column (current-column)))
      (dotimes (i (if (= num-lines 0) 1 num-lines))
        (mc/save-excursion
         (let ((furthest-cursor (cl-ecase direction
                                  (forwards  (mc/furthest-cursor-after-point))
                                  (backwards (mc/furthest-cursor-before-point)))))
           (when (overlayp furthest-cursor)
             (goto-char (overlay-get furthest-cursor 'point))
             (when (= num-lines 0)
               (mc/remove-fake-cursor furthest-cursor))))
         (cl-ecase direction
           (forwards (next-logical-line 1 nil))
           (backwards (previous-logical-line 1 nil)))
         (move-to-column cur-column)
         (mc/create-fake-cursor-at-point)))))
  (advice-add 'mc/mark-lines :override #'jcs--mc/mark-lines))

(use-package neotree
  :defer t
  :init
  (setq neo-window-position 'right
        neo-show-hidden-files t
        neo-window-width 35
        neo-toggle-window-keep-p nil
        neo-theme 'ascii
        neo-hide-cursor t
        neo-smart-open t)

  (defvar jcs--neotree--refresh-delay 0.3
    "Delay time after start refreshing `neotree'.")

  (defvar jcs--neotree--refresh-timer nil
    "Timer for refresh `neotree'.")

  (defvar jcs--neotree--last-window nil
    "Record last window before leaving `neotree'.")

  (defun jcs--neotree--kill-timer ()
    "Kill `neotree' refresh timer."
    (when (timerp jcs--neotree--refresh-timer)
      (cancel-timer jcs--neotree--refresh-timer)
      (setq jcs--neotree--refresh-timer nil)))

  (defun jcs--neotree-start-refresh ()
    "Start the refresh timer for `neotree'."
    (jcs--neotree--kill-timer)
    (setq jcs--neotree--refresh-timer
          (run-with-timer jcs--neotree--refresh-delay nil
                          #'jcs--neotree-refresh)))

  (defun jcs--neotree-refresh ()
    "Safe refresh `neotree'."
    (when (and (functionp 'neotree-refresh) (neo-global--window-exists-p))
      (save-selected-window (neotree-refresh))))

  (defun jcs--neotree-toggle--advice-around (fnc &rest args)
    "Advice execute around `neotree-toggle' command."
    (unless (neo-global--window-exists-p)
      (setq jcs--neotree--last-window (selected-window)))
    (apply fnc args)
    (unless (neo-global--window-exists-p)
      (select-window jcs--neotree--last-window)))
  (advice-add 'neotree-toggle :around #'jcs--neotree-toggle--advice-around)
  :config
  (defun jcs--neo-after-create-hook (&rest _)
    "Hooks called after creating the neotree buffer."
    (buffer-wrap-mode 1))
  (add-hook 'neo-after-create-hook 'jcs--neo-after-create-hook))

(use-package origami
  :defer t
  :config
  (set-face-attribute 'origami-fold-replacement-face nil
                      :foreground "#808080"
                      :box '(:line-width -1 :style 'pressed-button))
  (global-origami-mode t))

(use-package popup
  :defer t
  :config
  (defvar jcs-popup-mouse-events-flag-p nil
    "Check if `popup-menu-item-of-mouse-event' is called.")

  (defvar jcs-popup-selected-item-flag-p nil
    "Check if `popup-selected-item' is called.")

  (defun jcs-popup-clicked-on-menu-p ()
    "Check if the user actually clicked on the `popup' object."
    (and jcs-popup-mouse-events-flag-p
         (not jcs-popup-selected-item-flag-p)))

  (defun jcs--popup-menu-item-of-mouse-event--advice-after (event)
    "Advice after execute `popup-menu-item-of-mouse-event' command."
    (setq jcs-popup-mouse-events-flag-p t)
    (setq jcs-popup-selected-item-flag-p nil))
  (advice-add 'popup-menu-item-of-mouse-event :after #'jcs--popup-menu-item-of-mouse-event--advice-after)

  (defun jcs--popup-selected-item--advice-after (popup)
    "Advice after execute `popup-selected-item' command."
    (setq jcs-popup-selected-item-flag-p (jcs-last-input-event-p "mouse-1")))
  (advice-add 'popup-selected-item :after #'jcs--popup-selected-item--advice-after)

  (defun jcs--popup-draw--advice-around (orig-fun &rest args)
    "Advice around execute `popup-draw' command."
    (let ((do-orig-fun t))
      (when (and (jcs-last-input-event-p "mouse-1")
                 (not (jcs-popup-clicked-on-menu-p)))
        (keyboard-quit)
        (setq do-orig-fun nil))
      (when do-orig-fun (apply orig-fun args))))
  (advice-add 'popup-draw :around #'jcs--popup-draw--advice-around))

(use-package powerline
  :defer t
  :init
  ;; NOTE:
  ;; The separator to use for the default theme.
  ;;
  ;; Valid Values: alternate, arrow, arrow-fade, bar, box,
  ;; brace, butt, chamfer, contour, curve, rounded, roundstub,
  ;; wave, zigzag, utf-8.
  (setq powerline-default-separator 'wave))

(use-package preproc-font-lock
  :defer t
  :init
  (setq preproc-font-lock-modes '(cc-mode c-mode c++-mode csharp-mode nasm-mode))
  :config
  (set-face-attribute 'preproc-font-lock-preprocessor-background nil
                      :background nil :foreground "#B363BE" :inherit nil))

(use-package projectile
  :defer t
  :init
  (setq projectile-completion-system 'ivy
        projectile-current-project-on-switch 'keep)
  :config
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '(".log" ".vs" ".vscode" "node_modules")))
  (add-hook 'projectile-after-switch-project-hook #'jcs-dashboard-refresh-buffer))

(use-package quelpa
  :defer t
  :config
  (add-hook 'quelpa-before-hook (lambda () (setq jcs-package-installing-p t)))
  (add-hook 'quelpa-after-hook (lambda () (setq jcs-package-installing-p nil))))

(use-package region-occurrences-highlighter
  :defer t
  :init
  (setq region-occurrences-highlighter-min-size 1)
  :config
  (set-face-attribute 'region-occurrences-highlighter-face nil
                      :background "#113D6F" :inverse-video nil))

(use-package reload-emacs
  :defer t
  :init
  (setq reload-emacs-load-path '("~/.emacs.jcs/"
                                 "~/.emacs.jcs/func/"
                                 "~/.emacs.jcs/mode/"))
  :config
  (defun jcs--reload-emacs-after-hook ()
    "Hook runs after reload Emacs."
    (jcs-re-enable-mode 'company-fuzzy-mode))
  (add-hook 'reload-emacs-after-hook #'jcs--reload-emacs-after-hook))

(use-package right-click-context
  :defer t
  :config
  ;;;###autoload
  (defun right-click-context-menu ()
    "Open Right Click Context menu."
    (interactive)
    (let ((popup-menu-keymap (copy-sequence popup-menu-keymap)))
      (define-key popup-menu-keymap [mouse-3] #'right-click-context--click-menu-popup)
      (let ((value (popup-cascade-menu (right-click-context--build-menu-for-popup-el (right-click-context--menu-tree) nil))))
        (when (and (jcs-popup-clicked-on-menu-p) value)
          (if (symbolp value) (call-interactively value t) (eval value)))))))

(use-package searcher
  :defer t
  :init
  (setq searcher-search-type 'regex  ; `regex' or `flx'
        searcher-flx-threshold 25))

(use-package show-eol
  :defer t
  :config
  (show-eol-set-mark-with-string 'newline-mark "¶")

  (defun jcs-advice-show-eol-enable-before ()
    "Advice before execute `show-eol-enable' command."
    (face-remap-add-relative 'whitespace-newline :inverse-video t))
  (advice-add 'show-eol-enable :before #'jcs-advice-show-eol-enable-before)

  (defun jcs-advice-show-eol-disable-before ()
    "Advice before execute `show-eol-disable' command."
    (face-remap-add-relative 'whitespace-newline :inverse-video nil))
  (advice-add 'show-eol-disable :before #'jcs-advice-show-eol-disable-before))

(use-package sql-indent
  :defer t
  :init
  ;; URL: https://www.emacswiki.org/emacs/SqlIndent

  ;; 1 = 2 spaces,
  ;; 2 = 4 spaces,
  ;; 3 = 6 spaces,
  ;; n = n * 2 spaces,
  ;; etc.
  (setq sql-indent-offset 1))

(use-package un-mini
  :defer t
  :init
  (setq un-mini-abort-commands '(right-click-context-click-menu)))

(use-package undo-tree
  :defer t
  :config
  (global-undo-tree-mode t))

(use-package use-ttf
  :defer t
  :init
  ;; List of TTF fonts you want to use in the currnet OS.
  (setq use-ttf-default-ttf-fonts '(;; >> Classic Console <<
                                    "/.emacs.jcs/fonts/clacon.ttf"
                                    ;; >> Ubuntu Mono <<
                                    "/.emacs.jcs/fonts/UbuntuMono-R.ttf"))
  ;; Name of the font we want to use as default.
  ;; This you need to check the font name in the system manually.
  (setq use-ttf-default-ttf-font-name "Ubuntu Mono"))

(use-package web-mode
  :defer t
  :init
  ;; Associate an engine
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  (setq web-mode-content-types-alist
        '(("json" . "/some/path/.*\\.api\\'")
          ("xml"  . "/other/path/.*\\.api\\'")
          ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))


  ;; Quotation Mark
  (setq web-mode-auto-quote-style 1)  ; 1, for double quotes; 2, for single quotes

  ;; Indentation
  ;; NOTE: HTML element offset indentation
  (setq web-mode-markup-indent-offset 2)
  ;; NOTE: CSS offset indentation
  (setq web-mode-css-indent-offset 2)
  ;; NOTE: Script/code offset indentation (for JavaScript,
  ;;                                           Java,
  ;;                                           PHP,
  ;;                                           Ruby,
  ;;                                           Go,
  ;;                                           VBScript,
  ;;                                           Python, etc.)
  (setq web-mode-code-indent-offset 2)

  ;; Left padding
  (setq web-mode-style-padding 2  ; For `<style>' tag
        web-mode-script-padding 2  ; For `<script>' tag
        web-mode-block-padding 0)  ; For `php', `ruby', `java', `python', `asp', etc.

  ;; Offsetless Elements
  ;; NOTE: Do not make these lists to one list.
  ;; They are totally different list.
  ;; NOTE: This variable is from `web-mode' itself.
  (setq web-mode-offsetless-elements '("html"))

  ;; NOTE: Do not make these lists to one list.
  ;; They are totally different list.
  (defvar jcs-web-mode-offsetless-elements-toggle '("html")
    "List of HTML elements you want to be toggable to the
`wen-mode-offsetless-elements' list in Web mode.")

  ;; Comments
  ;;(setq web-mode-comment-style 2)

  ;; Snippets
  (setq web-mode-extra-snippets
        '(("erb" . (("toto" . ("<% toto | %>\n\n<% end %>"))))
          ("php" . (("dowhile" . ("<?php do { ?>\n\n<?php } while (|); ?>"))
                    ("debug" . ("<?php error_log(__LINE__); ?>"))))))

  ;; Auto-pairs
  (setq web-mode-extra-auto-pairs
        '(("erb"  . (("beg" "end")))
          ("php"  . (("beg" "end")
                     ("beg" "end")))))

  ;; Enable / disable features
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-comment-keywords t
        web-mode-enable-heredoc-fontification t)

  ;; Keywords / Constants
  ;;(setq web-mode-extra-constants '(("php" . ("CONS1" "CONS2"))))

  ;; Highlight current HTML element
  (setq web-mode-enable-current-element-highlight t)

  ;; You can also highlight the current column with
  (setq web-mode-enable-current-column-highlight t))

(use-package which-key
  :defer t
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
        which-key-idle-delay 1.0))

(use-package whitespace
  :defer t
  :config
  (autoload 'whitespace-mode "whitespace-mode" "Toggle whitespace visualization." t)
  (autoload 'whitespace-toggle-options "whitespace-mode" "Toggle local `whitespace-mode' options." t)
  ;; All the face can be find here.
  ;; URL: https://www.emacswiki.org/emacs/BlankMode
  (set-face-attribute 'whitespace-indentation nil
                      :background "grey20" :foreground "aquamarine3")
  (set-face-attribute 'whitespace-trailing nil
                      :background "grey20" :foreground "red"))

(use-package windmove
  :defer t
  :init
  (setq windmove-wrap-around t))

(use-package yascroll
  :defer t
  :init
  (setq yascroll:delay-to-hide 0.8))

(use-package yasnippet
  :defer t
  :config
  (require 'yasnippet-snippets)
  (yas-global-mode 1))

(provide 'jcs-plugin)
;;; jcs-plugin.el ends here
