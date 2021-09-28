;;; jcs-plugin.el --- Plugin Configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf adaptive-wrap
  :init
  (add-hook 'visual-line-mode-hook (lambda () (adaptive-wrap-prefix-mode +1))))

(leaf atl-markup
  :init
  (setq atl-markup-delay 0.2))

(leaf auto-highlight-symbol
  :init
  (setq ahs-idle-interval 0.15))

(leaf auto-read-only
  :defer-config
  (add-to-list 'auto-read-only-file-regexps "/[.]emacs[.]d/elisp/")
  (add-to-list 'auto-read-only-file-regexps "/[.]emacs[.]d/elpa/")
  (add-to-list 'auto-read-only-file-regexps "/lisp/")

  (defun jcs--auto-read-only--hook-find-file ()
    "Advice override function `auto-read-only--hook-find-file'."
    (when (and (not jcs-package-installing-p) (not (jcs-project-current)))
      (auto-read-only)))
  (advice-add 'auto-read-only--hook-find-file :override #'jcs--auto-read-only--hook-find-file))

(leaf auto-rename-tag
  :init
  (setq auto-rename-tag-disabled-commands '(query-replace)
        auto-rename-tag-disabled-minor-modes '(iedit-mode
                                               multiple-cursors-mode)))

(leaf better-scroll
  :init
  (setq better-scroll-align-type 'relative
        better-scroll-allow-boundary-movement t))

(leaf browse-kill-ring
  :init
  (setq browse-kill-ring-separator-face 'font-lock-comment-face)
  :defer-config
  (defun jcs--browse-kill-ring-mode-hook ()
    "Hook for `browse-kill-ring-mode'."
    (setq browse-kill-ring-separator (jcs-env-separator))
    (page-break-lines-mode 1))
  (add-hook 'browse-kill-ring-mode-hook 'jcs--browse-kill-ring-mode-hook))

(leaf buffer-wrap
  :defer-config
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

(leaf centaur-tabs
  :init
  (setq centaur-tabs-set-icons nil
        centaur-tabs-style "wave"
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"))

(leaf company
  :init
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-require-match nil
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-eclim-auto-save nil
        company-minimum-prefix-length 0
        company-idle-delay 0.07
        company-selection-wrap-around 'on
        company-format-margin-function #'company-detect-icons-margin)
  (setq company-backends
        (append
         '(company-capf company-semantic)
         '(company-keywords)
         '(company-abbrev company-dabbrev company-dabbrev-code)
         '(company-files)
         '(company-etags company-gtags)
         '(company-yasnippet)))
  :defer-config
  (with-eval-after-load 'company (require 'jcs-company) (global-company-mode t)))

(leaf company-box
  :hook (company-mode-hook . company-box-mode)
  :init
  (setq company-box-backends-colors nil
        company-box-frame-behavior 'point
        company-box-doc-delay 0.3
        company-box-doc-text-scale-level -2))

(leaf company-c-headers
  :defer-config
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

(leaf company-emojify
  :init
  (setq company-emojify-annotation 'image
        company-emojify-emoji-styles '(github)))

(leaf company-fuzzy
  :hook (company-mode-hook . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-history-backends '(company-yasnippet)
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'")))

(leaf counsel
  :init
  (defvar jcs-counsel-find-file-ignore '("[.]meta$" "/node_modules/")
    "List of find file ignore regexp string.")
  (setq counsel-preselect-current-file t
        counsel-find-file-at-point t
        counsel-find-file-ignore-regexp (mapconcat
                                         (lambda (elm) (concat "\\(" elm "\\)"))
                                         jcs-counsel-find-file-ignore "\\|")))

(leaf csharp-mode
  :init
  (setq csharp-codedoc-tag-face 'font-lock-doc-face))

(leaf dashboard
  :init
  (setq dashboard-banner-logo-title "[J C S • E M A C S]"
        dashboard-footer-icon ""
        dashboard-footer-messages
        `(,(format "Copyright (c) %s %s" (jcs-year-only) (jcs-copyright-info)))
        dashboard-items '((recents . 10)
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
        dashboard-projects-backend 'project-el
        ;; Truncate style
        dashboard-path-style 'truncate-middle
        dashboard-recentf-show-base 'align
        dashboard-projects-show-base 'align
        dashboard-bookmarks-show-base 'align
        dashboard-bookmarks-item-format "%s  %s"
        dashboard-shorten-by-window-width t
        dashboard-shorten-path-offset 15)
  :defer-config
  (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (require 'dashboard-ls)
  (dashboard-setup-startup-hook))

(leaf dashboard-ls
  :defer-config
  (let ((dashboard-lst-items '((ls-directories . 5) (ls-files . 5))))
    (setq dashboard-items (append dashboard-lst-items dashboard-items))))

(leaf define-it
  :init
  (setq define-it-output-choice 'view))

(leaf diff-hl
  :init
  (setq diff-hl-side 'right))

(leaf diminish
  :defer-config
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
    (if load-sym (with-eval-after-load load-sym (jcs-diminish-type mode-sym))
      (jcs-diminish-type mode-sym)))

  (defun jcs-diminish-do-alist (alst)
    "Diminish the whole ALST."
    (dolist (type alst) (jcs-diminish (car type) (cdr type))))

  (defconst jcs-diminish-alist
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
      (company-box-mode . company-box)
      (company-fuzzy-mode . company-fuzzy)
      (docstr-mode . docstr)
      (editorconfig-mode . editorconfig)
      (eldoc-mode)
      (elm-indent-mode . elm-mode)
      (emmet-mode . emmet-mode)
      (buffer-face-mode . face-remap)
      (fill-page-mode . fill-page)
      (flycheck-mode . flycheck)
      (helm-mode . helm-mode)
      (hi-lock-mode . hi-lock)
      (highlight-indent-guides-mode . highlight-indent-guides)
      (hl-preproc-mode . hl-preproc)
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
      (tree-sitter-mode . tree-sitter)
      (un-mini-mode . un-mini)
      (undo-tree-mode . undo-tree)
      (view-mode . view)
      (visual-line-mode)
      (which-key-mode . which-key)
      ((whitespace-mode whitespace-newline-mode) . whitespace)
      (with-editor-mode . with-editor)
      (yas-minor-mode . yasnippet))
    "List of diminish associated list.")

  (jcs-diminish-do-alist jcs-diminish-alist))

(leaf diminish-buffer
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
         '("[*]lsp-" "[*]LSP[ ]+"
           "[*][a-zA-Z0-9]+[-]*ls" "[*][a-zA-Z0-9]+::stderr[*]"
           "[*]csharp[*]")  ; From `lsp'
         '("[*]tree-sitter" "tree-sitter-tree:")
         '("[*]company")
         '("[*]Local Variables[*]")
         '("[*]Kill Ring[*]")  ; From `browse-kill-ring'
         '("[*]SPEEDBAR")
         '("[*]Flycheck" "[*]Flymake log[*]")
         '("[*]httpd[*]")
         '("[*]helpful")
         '("[*]ert[*]")  ; Emacs Lisp Regression Testing
         '("magit[-]*[[:ascii:]]*[:]")  ; From `magit'
         '("[*]Most used words[*]")
         '("[*]Test SHA[*]")
         '("[*]RE-Builder")
         '("[*]preview-it")
         '("[*]wclock[*]")
         '("[*]Clippy[*]")
         '("[*]CMake Temporary[*]")
         '("[*]org-src-fontification")))
  (setq diminish-buffer-mode-list
        (append
         '("Dired by name")))
  (with-eval-after-load 'jcs-buffer-menu (diminish-buffer-mode 1))
  :defer-config
  (defun jcs--diminish-buffer-clean--advice-before ()
    "Advice do clean buffer."
    (when diminish-buffer-mode (diminish-buffer-clean)))
  (advice-add 'jcs-buffer-menu-refresh-buffer :before #'jcs--diminish-buffer-clean--advice-before))

(leaf display-fill-column-indicator
  :init
  (setq-default display-fill-column-indicator-column 80)
  :defer-config
  (jcs--set-common-face 'fill-column-indicator "#AA4242"))

(leaf docstr
  :init
  (setq docstr-key-support t
        docstr-desc-summary ""))

(leaf dumb-jump
  :init
  (setq dumb-jump-selector 'ivy))

(leaf elisp-def
  :init
  (defvar jcs-elisp-def-modes '(emacs-lisp-mode lisp-mode lisp-interaction-mode)
    "List of `major-mode' that works with `elisp-def'."))

(leaf elisp-demos
  :init
  (with-eval-after-load 'help-fns
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

(leaf emojify
  :init
  (setq emojify-emoji-styles '(github)
        emojify-company-tooltips-p t))

(leaf eshell-syntax-highlighting
  :init
  (with-eval-after-load 'eshell
    (eshell-syntax-highlighting-global-mode +1)))

(leaf eww
  :init
  (setq eww-search-prefix "https://www.google.com/search?q="))

(leaf exec-path-from-shell
  :defer-config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(leaf feebleline
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
  :defer-config
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

(leaf ffmpeg-player
  :init
  (setq ffmpeg-player--volume 75
        ffmpeg-player-display-width 672
        ffmpeg-player-display-height 378
        ffmpeg-player-no-message t)
  :defer-config
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

(leaf file-header
  :init
  (setq file-header-template-config-filepath "~/.emacs.jcs/template/template_config.properties"))

(leaf flycheck-grammarly
  :hook (flycheck-mode-hook . (lambda () (require 'flycheck-grammarly))))

(leaf flycheck-languagetool
  :hook (flycheck-mode-hook . (lambda () (require 'flycheck-languagetool))))

(leaf flycheck-popup-tip
  :init
  (defun jcs--flycheck-mode--pos-tip--advice-after (&rest _)
    "Advice runs after `flycheck-mode' function with `flycheck-popup-tip'."
    (jcs-enable-disable-mode-by-condition 'flycheck-popup-tip-mode
                                          (and (not (display-graphic-p)) flycheck-mode)))
  (advice-add 'flycheck-mode :after #'jcs--flycheck-mode--pos-tip--advice-after))

(leaf flycheck-pos-tip
  :init
  (defun jcs--flycheck-mode--pos-tip--advice-after (&rest _)
    "Advice runs after `flycheck-mode' function with `flycheck-pos-tip'."
    (jcs-enable-disable-mode-by-condition 'flycheck-pos-tip-mode
                                          (and (display-graphic-p) flycheck-mode)))
  (advice-add 'flycheck-mode :after #'jcs--flycheck-mode--pos-tip--advice-after))

(leaf google-translate
  :init
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh-TW")
  :defer-config
  (defun jcs--google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  (advice-add 'google-translate--search-tkk :override #'jcs--google-translate--search-tkk))

(leaf goto-char-preview
  :defer-config
  (advice-add 'goto-char-preview :after #'jcs--recenter--advice-after))

(leaf goto-line-preview
  :defer-config
  (advice-add 'goto-line-preview :after #'jcs--recenter--advice-after))

(leaf highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-responsive 'top))

(leaf highlight-numbers
  :defer-config
  (jcs--set-common-face 'highlight-numbers-number "#9BCEA3"))

(leaf hl-todo
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
  :defer-config
  (defun jcs--hl-todo--inside-comment-or-string-p ()
    "Redefine `hl-todo--inside-comment-or-string-p', for accurate highlighting."
    (jcs-inside-comment-or-string-p))
  (advice-add #'hl-todo--inside-comment-or-string-p :override #'jcs--hl-todo--inside-comment-or-string-p))

(leaf impatient-showdown
  :init
  (setq impatient-showdown-flavor 'github))

(leaf indent-control
  :init
  (setq indent-control-records
        '((actionscript-mode     . 4)
          (c-mode                . 4)
          (c++-mode              . 4)
          (csharp-mode           . 4)
          (css-mode              . 2)
          (dockerfile-mode       . 2)
          (elisp-mode            . 2)
          (emacs-lisp-mode       . 2)
          (go-mode               . 4)
          (groovy-mode           . 4)
          (java-mode             . 4)
          (jayces-mode           . 4)
          (js-mode               . 2)
          (js2-mode              . 2)
          (json-mode             . 2)
          (kotlin-mode           . 4)
          (less-css-mode         . 2)
          (lisp-mode             . 2)
          (lisp-interaction-mode . 2)
          (lua-mode              . 4)
          (nasm-mode             . 4)
          (nix-mode              . 2)
          (nxml-mode             . 2)
          (objc-mode             . 4)
          (python-mode           . 4)
          (rjsx-mode             . 2)
          (ruby-mode             . 2)
          (rust-mode             . 4)
          (scss-mode             . 2)
          (shader-mode           . 4)
          (ssass-mode            . 2)
          (sql-mode              . 1)
          (typescript-mode       . 4)
          (web-mode              . 2)
          (yaml-mode             . 2))))

(leaf isearch
  :init
  (setq isearch-lazy-count t
        lazy-count-prefix-format "[%s:%s] ")
  :defer-config
  (add-hook 'isearch-mode-hook #'jcs-scroll-conservatively-disable)
  (add-hook 'isearch-mode-end-hook #'jcs-scroll-conservatively-enable))

(leaf isearch-project
  :init
  (setq isearch-project-ignore-paths '(".vs/"
                                       ".vscode/"
                                       "bin/"
                                       "build/"
                                       "build.min/"
                                       "node_modules/"
                                       "res/"))
  :defer-config
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

(leaf ivy
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
    "Ratio that respect to `frame-height' and `ivy-height'.")
  :defer-config
  (require 'smex)
  (setq enable-recursive-minibuffers t))

(leaf ivy-file-preview
  :hook (ivy-mode-hook . ivy-file-preview-mode)
  :init
  (setq ivy-file-preview-overlay-delay-time 0.2))

(leaf ivy-searcher
  :init
  (setq ivy-searcher-display-info 'line/column
        ivy-searcher-preselect 'next)
  :defer-config
  (advice-add 'ivy-searcher-replace-file :after #'jcs-revert-all-buffers)
  (advice-add 'ivy-searcher-replace-project :after #'jcs-revert-all-buffers)
  (advice-add 'ivy-searcher-search-file :after #'jcs--recenter--advice-after)
  (advice-add 'ivy-searcher-search-project :after #'jcs--recenter--advice-after))

(leaf keypression
  :defer-config
  (setq keypression-ignore-mouse-events
        (append keypression-ignore-mouse-events
                '(switch-frame menu-bar tool-bar tab-bar))))

(leaf line-reminder
  :init
  (setq line-reminder-show-option (if (display-graphic-p) 'indicators 'linum))
  (unless (display-graphic-p)
    (setq line-reminder-saved-sign " |"
          line-reminder-modified-sign " |")))

(leaf logms
  :defer-config
  (logms-mode 1))

(leaf lsp-mode
  :init
  (setq lsp-auto-guess-root t
        lsp-prefer-capf t
        lsp-keep-workspace-alive nil                   ; Auto-kill LSP server
        lsp-modeline-code-action-fallback-icon "|Œ|"
        lsp-prefer-flymake nil                         ; Use lsp-ui and flycheck
        flymake-fringe-indicator-position 'right-fringe)

  (defun jcs--lsp-connected-p ()
    "Check if LSP connected."
    (if (boundp 'lsp-managed-mode) lsp-managed-mode nil))

  (defun jcs--safe-lsp-active ()
    "Safe way to active LSP."
    (require 'jcs-project)
    (when (and (jcs-project-under-p) (not (jcs--lsp-connected-p)))
      (lsp-deferred))))

(leaf lsp-ui
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-text-scale-level -1
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-delay 0.6
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
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
    (when (functionp 'lsp-ui-doc--hide-frame) (lsp-ui-doc--hide-frame)))
  :defer-config
  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (lsp-ui-sideline-set-default-icon))

(leaf meta-view
  :defer-config
  (defun jcs--meta-view-after-insert-hook ()
    "Hook runs after meta-view buffer insertion."
    (jcs-prog-mode-hook)
    (display-line-numbers-mode 1)
    (setq-local origami-show-summary nil)
    (jcs-save-excursion  ; fold all comments
      (goto-char (point-min))
      (call-interactively #'origami-close-node)
      (let (continuation)
        (while (not (eobp))
          (forward-line 1)
          (end-of-line)
          (if (jcs-inside-comment-p)
              (unless continuation
                (call-interactively #'origami-close-node)
                (setq continuation t))
            (setq continuation nil))))))
  (add-hook 'meta-view-after-insert-hook #'jcs--meta-view-after-insert-hook))

(leaf most-used-words
  :init
  (setq most-used-words-display-type 'table
        most-used-words-word-display 100))

(leaf modablist
  :defer-config
  (set-face-attribute 'modablist-select-face nil
                      :box '(:line-width -1 :color "#65A7E2" :style nil))
  (set-face-attribute 'modablist-insert-face nil :background "#565136"
                      :box '(:line-width -1 :color "#65A7E2" :style nil)))

(leaf multi-shell
  :init
  (setq multi-shell-prefer-shell-type 'shell))  ; Accept `shell' or `eshll'.

(leaf multiple-cursors
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
  :defer-config
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

(leaf origami
  :init
  (setq origami-indicators-fringe 'left-fringe
        origami-indicators-time 0.2
        origami-indicators-face-function #'jcs--origami-indicators-face-function)
  :defer-config
  (require 'line-reminder)
  (defun jcs--origami-indicators-face-function (pos &rest _)
    "Return the face of it's function."
    (let ((ln (line-number-at-pos pos)))
      (cond
       ((jcs-contain-list-integer line-reminder--change-lines ln)
        'line-reminder-modified-sign-face)
       ((jcs-contain-list-integer line-reminder--saved-lines ln)
        'line-reminder-saved-sign-face)
       (t nil))))

  (defun jcs--origami-indicators--refresh ()
    "Refresh indicators for package `origmai'."
    (when origami-indicators-mode (origami-indicators--refresh (current-buffer))))
  (advice-add 'line-reminder-transfer-to-saved-lines :after #'jcs--origami-indicators--refresh)

  (set-face-attribute 'origami-fold-replacement-face nil
                      :foreground "#808080"
                      :box '(:line-width -1 :style 'pressed-button)))

(leaf popup
  :defer-config
  (defvar jcs-popup-mouse-events-flag-p nil
    "Check if `popup-menu-item-of-mouse-event' is called.")

  (defvar jcs-popup-selected-item-flag-p nil
    "Check if `popup-selected-item' is called.")

  (defun jcs-popup-clicked-on-menu-p ()
    "Check if the user actually clicked on the `popup' object."
    (and jcs-popup-mouse-events-flag-p
         (not jcs-popup-selected-item-flag-p)))

  (defun jcs--popup-menu-item-of-mouse-event--advice-after (event)
    "Advice after execute command `popup-menu-item-of-mouse-event'."
    (setq jcs-popup-mouse-events-flag-p t
          jcs-popup-selected-item-flag-p nil))
  (advice-add 'popup-menu-item-of-mouse-event :after #'jcs--popup-menu-item-of-mouse-event--advice-after)

  (defun jcs--popup-selected-item--advice-after (popup)
    "Advice after execute command `popup-selected-item'."
    (setq jcs-popup-selected-item-flag-p (jcs-last-input-event-p "mouse-1")))
  (advice-add 'popup-selected-item :after #'jcs--popup-selected-item--advice-after)

  (defun jcs--popup-draw--advice-around (orig-fun &rest args)
    "Advice around execute command `popup-draw'."
    (let ((do-orig-fun t))
      (when (and (jcs-last-input-event-p "mouse-1")
                 (not (jcs-popup-clicked-on-menu-p)))
        (keyboard-quit)
        (setq do-orig-fun nil))
      (when do-orig-fun (apply orig-fun args))))
  (advice-add 'popup-draw :around #'jcs--popup-draw--advice-around))

(leaf pos-tip
  :init
  (setq pos-tip-internal-border-width 5))

(leaf powerline
  :init
  (setq powerline-default-separator 'wave)
  :defer-config
  (defun jcs--powerline-raw--advice-around (fnc &rest args)
    "Advice execute around function `powerline-raw'."
    (let ((str (nth 0 args)))
      (when (stringp str)
        (setq str (jcs-s-replace-displayable str))
        (setf (nth 0 args) str)))
    (apply fnc args))
  (advice-add 'powerline-raw :around #'jcs--powerline-raw--advice-around)

  ;; Override
  (defpowerline powerline-vc
    (when (jcs-vc-status)
      (format " %s%s" (jcs-vc-project) (jcs-vc-info)))))

(leaf project
  :defer-config
  (setq project-vc-ignores
        (append project-vc-ignores
                '(".idea" ".vscode"
                  ".ensime_cache" ".eunit"
                  ".git" ".hg" ".fslckout"
                  "_FOSSIL_" ".bzr" "_darcs"
                  ".tox" ".svn"
                  ".stack-work" ".ccls-cache" ".cache" ".clangd")
                '(".log" ".vs" "node_modules"))))

(leaf quelpa
  :defer-config
  (add-hook 'quelpa-before-hook (lambda () (setq jcs-package-installing-p t)))
  (add-hook 'quelpa-after-hook (lambda () (setq jcs-package-installing-p nil))))

(leaf quick-peek
  :init
  (defun jcs-quick-peek--form-face (fg &optional weight)
    "Form `quick-peek' face with FG."
    (unless weight (setq weight 'normal))
    (let ((color (or (face-attribute 'highlight :background) "black")))
      `(:background ,color :foreground ,fg :inherit quick-peek-border-face :weight ,weight)))

  (defun jcs-set-quick-peek-spacers (buf ln)
    "Prepare quick peek header and footer."
    (let ((default-face (jcs-quick-peek--form-face "white")))
      (setq jcs-quick-peek--spacer-header
            (concat
             (if (jcs-last-line-in-buffer-p) "\n" "")
             (propertize " " 'face default-face)
             (propertize (buffer-name buf) 'face (jcs-quick-peek--form-face "black" 'bold))
             (propertize " " 'face default-face)
             (propertize (buffer-file-name buf) 'face (jcs-quick-peek--form-face "#222"))
             (propertize "\n" 'face default-face))
            jcs-quick-peek--spacer-footer
            (propertize (concat (jcs-env-separator)
                                (if (eq quick-peek-position 'below) "" "\n"))
                        'face default-face))))

  (defun jcs-quick-peek--scroll-to-see ()
    "Scroll buffer in order to see the full `quick-peek' content."
    (let ((default-max-h 16) (ln-current (line-number-at-pos)) lvl ln-diff)
      (when (eq quick-peek-position 'below)
        (setq lvl (jcs-last-visible-line-in-window)
              ln-diff (- lvl ln-current))
        (when (< ln-diff default-max-h)
          (jcs-scroll-up-line (- default-max-h ln-diff))))))
  :defer-config
  (defvar jcs-quick-peek--spacer-header nil
    "Header string for `quick-peek'")
  (defvar jcs-quick-peek--spacer-footer nil
    "Footer string for `quick-peek'.")
  (defun jcs--quick-peek--insert-spacer--advice-override (pos str-before str-after)
    "Advice exection override function `quick-peek--insert-spacer'."
    (let ((str (if (= pos (point-min)) jcs-quick-peek--spacer-header
                 jcs-quick-peek--spacer-footer)))
      (save-excursion (goto-char pos) (insert str))))
  (advice-add 'quick-peek--insert-spacer :override #'jcs--quick-peek--insert-spacer--advice-override))

(leaf region-occurrences-highlighter
  :init
  (setq region-occurrences-highlighter-min-size 1)
  :defer-config
  (set-face-attribute 'region-occurrences-highlighter-face nil
                      :background "#113D6F" :inverse-video nil))

(leaf right-click-context
  :defer-config
  (defun right-click-context-menu ()
    "Open Right Click Context menu."
    (interactive)
    (let ((popup-menu-keymap (copy-sequence popup-menu-keymap)))
      (define-key popup-menu-keymap [mouse-3] #'right-click-context--click-menu-popup)
      (let ((value (popup-cascade-menu (right-click-context--build-menu-for-popup-el (right-click-context--menu-tree) nil))))
        (when (and (jcs-popup-clicked-on-menu-p) value)
          (if (symbolp value) (call-interactively value t) (eval value)))))))

(leaf searcher
  :init
  (setq searcher-search-type 'regex  ; `regex' or `flx'
        searcher-flx-threshold 25))

(leaf show-eol
  :defer-config
  (show-eol-set-mark-with-string 'newline-mark "¶")

  (defun jcs-advice-show-eol-enable-before ()
    "Advice before execute `show-eol-enable' command."
    (face-remap-add-relative 'whitespace-newline :inverse-video t))
  (advice-add 'show-eol-enable :before #'jcs-advice-show-eol-enable-before)

  (defun jcs-advice-show-eol-disable-before ()
    "Advice before execute `show-eol-disable' command."
    (face-remap-add-relative 'whitespace-newline :inverse-video nil))
  (advice-add 'show-eol-disable :before #'jcs-advice-show-eol-disable-before))

(leaf sql-indent
  :init
  ;; URL: https://www.emacswiki.org/emacs/SqlIndent

  ;; 1 = 2 spaces,
  ;; 2 = 4 spaces,
  ;; 3 = 6 spaces,
  ;; n = n * 2 spaces,
  ;; etc.
  (setq sql-indent-offset 1))

(leaf tree-sitter
  :defer-config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (jcs-funcall-fboundp #'jcs-reset-common-faces-by-theme)

  (defconst jcs--tree-sitter-langs--query-repo
    "https://github.com/jcs090218/tree-sitter-queries"
    "Repository URL where stores all tree-sitter highlight queries.")

  (defun jcs--tree-sitter-grab-queries ()
    "Download all custom queries to the `tree-sitter-langs' queries folder."
    (require 'find-func)
    (let* ((default-directory (file-name-directory (find-library-name "tree-sitter-langs")))
           (repo-url (shell-quote-argument jcs--tree-sitter-langs--query-repo))
           (dirname (file-name-base jcs--tree-sitter-langs--query-repo))
           (clone-dir (expand-file-name dirname))
           (clone-queries (expand-file-name "queries" clone-dir))
           (dest-queries (expand-file-name "queries" default-directory))
           lang-dirs)
      (ignore-errors (delete-directory (expand-file-name dirname default-directory) t))
      (when (= 0 (shell-command (format "git clone %s" repo-url)))
        (setq lang-dirs (directory-files clone-queries))
        (pop lang-dirs) (pop lang-dirs)  ; remove . and ..
        (message "Installing custom tree-sitter query...")
        (dolist (lang-dir lang-dirs)
          (message "  - %s" lang-dir)
          (ignore-errors
            (delete-directory (expand-file-name lang-dir clone-queries)))
          (ignore-errors
            (copy-directory (expand-file-name lang-dir clone-queries)
                            (expand-file-name lang-dir dest-queries)
                            nil nil t)))
        (delete-directory clone-dir t)
        (message "Done install custom tree-sitter queries"))))

  (defun jcs--tree-sitter-hl-mode-hook ()
    "Hook for `tree-sitter-hl-mode'."
    (remove-hook 'tree-sitter-hl-mode-hook #'jcs--tree-sitter-hl-mode-hook)
    (jcs--tree-sitter-grab-queries)
    (tree-sitter-hl-mode 1))  ; re-enable it once
  (add-hook 'tree-sitter-hl-mode-hook #'jcs--tree-sitter-hl-mode-hook))

(leaf treemacs
  :init
  (setq treemacs-position 'right
        treemacs-missing-project-action 'remove
        treemacs-sorting 'alphabetic-asc
        treemacs-follow-after-init t
        treemacs-no-png-images t)
  :defer-config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  (defvar jcs-treemacs-width-ratio 0.15
    "Ratio that respect to `frame-width' and `neo-window-width'.")

  (defun jcs-treemacs-toggle-refresh ()
    "Refresh `treemacs' by toggle twice."
    (save-selected-window (treemacs) (treemacs)))

  (defun jcs-treemacs--window-size-change ()
    "`window-size-change-functions' for `treemacs'."
    (setq treemacs-width (round (* (frame-width) jcs-treemacs-width-ratio)))
    (when (treemacs-get-local-window) (jcs-treemacs-toggle-refresh)))

  (defun jcs--treemacs-mode-hook ()
    "Hook for `treemacs-mode'."
    (setq buffer-wrap--relative-max-line 0)
    (buffer-wrap-mode 1))
  (add-hook 'treemacs-mode-hook #'jcs--treemacs-mode-hook))

(leaf turbo-log
  :init
  (setq turbo-log-no-ask t))

(leaf un-mini
  :init
  (setq un-mini-abort-commands '(right-click-context-click-menu)))

(leaf undo-tree
  :defer-config
  (global-undo-tree-mode t))

(leaf use-ttf
  :init
  ;; List of TTF fonts you want to use in the currnet OS.
  (setq use-ttf-default-ttf-fonts '(;; >> Classic Console <<
                                    "/.emacs.jcs/fonts/clacon.ttf"
                                    ;; >> Ubuntu Mono <<
                                    "/.emacs.jcs/fonts/UbuntuMono-R.ttf"))
  ;; Name of the font we want to use as default.
  ;; This you need to check the font name in the system manually.
  (setq use-ttf-default-ttf-font-name "Ubuntu Mono"))

(leaf web-mode
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
  ;; NOTE: Script/code offset indentation (for JavaScript, Java, PHP, Ruby,
  ;; Go, VBScript, Python, etc.)
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

  ;; Highlight current HTML element
  (setq web-mode-enable-current-element-highlight t)

  ;; You can also highlight the current column with
  (setq web-mode-enable-current-column-highlight t))

(leaf which-key
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
        which-key-idle-delay 1.0))

(leaf windmove
  :init
  (setq windmove-wrap-around t))

(leaf yascroll
  :init
  (setq yascroll:delay-to-hide 0.8))

(leaf yasnippet
  :defer-config
  (require 'yasnippet-snippets)
  (yas-global-mode 1))

(provide 'jcs-plugin)
;;; jcs-plugin.el ends here
